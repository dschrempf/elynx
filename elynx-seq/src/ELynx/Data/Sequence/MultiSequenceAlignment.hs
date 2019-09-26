{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  ELynx.Data.MultiSequenceAlignment
Description :  Multi sequence alignment related types and functions
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable

Portability :  portable

Creation date: Thu Oct  4 18:40:18 2018.

This module is to be imported qualified.

-}


module ELynx.Data.Sequence.MultiSequenceAlignment
  ( MultiSequenceAlignment (MultiSequenceAlignment)
  , alphabet
  , length
  , nSequences
  -- | * Input, output
  , fromSequences
  , toSequences
  , toByteString
  , summarize
  -- | * Manipulation
  , join
  , concat
  , concatMSAs
  , filterColsOnlyStd
  , filterColsStd
  , filterColsNoGaps
  -- | * Analysis
  , FrequencyData
  , distribution
  , toFrequencyData
  , kEffEntropy
  , kEffHomoplasy
  , countIUPACChars
  , countGaps
  , countUnknowns
  -- | * Sub sample
  , subSample
  , randomSubSample
  ) where

import           Control.Lens
import           Control.Monad                             hiding (join)
import           Control.Monad.Primitive
import qualified Data.ByteString.Lazy.Char8                as L
import           Data.List                                 hiding (concat,
                                                            length)
import qualified Data.Matrix.Unboxed                       as M
import qualified Data.Vector.Unboxed                       as V
import           Prelude                                   hiding (concat,
                                                            length)
import           System.Random.MWC

import qualified ELynx.Data.Alphabet.Alphabet              as A
import           ELynx.Data.Alphabet.Character
import qualified ELynx.Data.Alphabet.DistributionDiversity as D
import           ELynx.Data.Sequence.Defaults
import qualified ELynx.Data.Sequence.Sequence              as S
import           ELynx.Tools.ByteString
import           ELynx.Tools.Concurrent
import           ELynx.Tools.Definitions
import           ELynx.Tools.Equality
import           ELynx.Tools.Matrix

-- | A collection of sequences.
data MultiSequenceAlignment = MultiSequenceAlignment
                              { _names    :: [S.Name]
                              , _alphabet :: A.Alphabet
                              , _matrix   :: M.Matrix Character
                              }
  deriving (Show, Eq)

makeLenses ''MultiSequenceAlignment

-- | Number of sites.
length :: MultiSequenceAlignment -> Int
length = M.cols . view matrix

-- | Number of sequences.
nSequences :: MultiSequenceAlignment -> Int
nSequences = M.rows . view matrix

-- | Create 'MultiSequenceAlignment' from a list of 'S.Sequence's.
fromSequences :: [S.Sequence] -> Either String MultiSequenceAlignment
fromSequences ss
  | S.equalLength ss && allEqual (map (view S.alphabet) ss) =
      Right $ MultiSequenceAlignment ns a d
  | S.equalLength ss =
      Left "Sequences do not have equal codes."
  | otherwise =
      Left "Sequences do not have equal lengths."
  where
    ns   = map (view S.name) ss
    a    = head ss ^. S.alphabet
    bss  = map (view S.characters) ss
    d    = M.fromRows bss

-- | Conversion to list of 'S.Sequence's.
toSequences :: MultiSequenceAlignment -> [S.Sequence]
toSequences (MultiSequenceAlignment ns a d) = zipWith (\n r -> S.Sequence n a r) ns rows
  where
    rows  = M.toRows d

-- | Show sequence of 'MultiSequenceAlignment' with given index, untrimmed.
toByteStringSeq :: MultiSequenceAlignment -> Int -> L.ByteString
toByteStringSeq m i =
  L.unwords [ alignLeft nameWidth $ (m ^. names) !! i
            , S.fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'MultiSequenceAlignment', untrimmed.
summarizeSeq :: MultiSequenceAlignment -> Int -> L.ByteString
summarizeSeq m i =
  L.unwords [ alignLeft nameWidth $ (m ^. names) !! i
            , summarizeByteString summaryLength $
              S.fromCharacters $ M.takeRow (m ^. matrix) i ]

tableHeader :: L.ByteString
tableHeader = L.unwords [ alignLeft nameWidth (L.pack "Name")
                      , L.pack "Sequence" ]

-- | Show a 'MultiSequenceAlignment' in text form.
toByteString :: MultiSequenceAlignment -> L.ByteString
toByteString msa = L.unlines $ tableHeader :
  map (toByteStringSeq msa) [0 .. (nSequences msa - 1)]

header :: MultiSequenceAlignment -> L.ByteString
header msa = L.unlines $
  [ L.pack "Multi sequence alignment."
  , L.pack $ "Code: " ++ A.description (msa^.alphabet) ++ "."
  , L.pack $ "Length: " ++ show (length msa) ++ "." ]
  ++ reportLengthSummary ++ reportNumberSummary
  where reportLengthSummary =
          [ L.pack $ "For each sequence, the "
            ++ show summaryLength ++ " first bases are shown."
          | length msa > summaryLength ]
        reportNumberSummary =
          [ L.pack $ show summaryNSequences ++ " out of " ++
            show (nSequences msa) ++ " sequences are shown."
          | nSequences msa > summaryNSequences ]

-- | Similar to 'S.summarizeSequenceList' but with different Header.
summarize :: MultiSequenceAlignment -> L.ByteString
summarize msa = L.unlines $ header msa :
  map (summarizeSeq msa) [0 .. n - 1]
  where n = min (nSequences msa) summaryNSequences

-- | Join two 'MultiSequenceAlignment's vertically. That is, add more sequences
-- to an alignment. See also 'msaConcatenate'.
join :: MultiSequenceAlignment
     -> MultiSequenceAlignment
     -> MultiSequenceAlignment
-- top bottom.
join t b
  | length t == length b
    && t^.alphabet == b^.alphabet
  = MultiSequenceAlignment ns a (tD === bD)
  | otherwise  = error "msaJoin: Multi sequence alignments do not have equal length."
  where
    ns = t^.names ++ b^.names
    tD = t^.matrix
    bD = t^.matrix
    a  = t^.alphabet

-- | Concatenate two 'MultiSequenceAlignment's horizontally. That is, add more
-- sites to an alignment. See also 'msaJoin'.
concat :: MultiSequenceAlignment
       -> MultiSequenceAlignment
       -> MultiSequenceAlignment
-- left right.
concat l r
  | nSequences l == nSequences r
    && l^.alphabet == r^.alphabet
  = MultiSequenceAlignment (l ^. names) a (lD ||| rD)
  | otherwise = error "msaConcatenate: Multi sequence alignments do not have equal length."
  where
    lD = l^.matrix
    rD = r^.matrix
    a  = l^.alphabet

-- | Concatenate a list of 'MultiSequenceAlignment's horizontally. See
-- 'msaConcatenate'.
concatMSAs :: [MultiSequenceAlignment] -> MultiSequenceAlignment
concatMSAs []    = error "msasConcatenate: Nothing to concatenate."
concatMSAs [msa] = msa
concatMSAs msas  = foldl' concat (head msas) (tail msas)

-- Only keep columns from alignment that satisfy given predicate.
filterColsWith :: (V.Vector Character -> Bool) -> MultiSequenceAlignment -> MultiSequenceAlignment
filterColsWith p = over matrix (M.fromColumns . filter p . M.toColumns)

-- | Only keep columns with standard characters. Alignment columns with IUPAC
-- characters are removed.
filterColsOnlyStd :: MultiSequenceAlignment -> MultiSequenceAlignment
filterColsOnlyStd msa = filterColsWith (V.all $ A.isStd (msa^.alphabet)) msa

-- | Filter columns with proportion of standard character larger than given number.
filterColsStd :: Double -> MultiSequenceAlignment -> MultiSequenceAlignment
filterColsStd prop msa = filterColsWith
  (\col -> prop * n <= fromIntegral (V.length (V.filter (A.isStd a) col)))
  msa
  where a = msa^.alphabet
        n = fromIntegral $ nSequences msa

-- | Only keep columns without gaps or unknown characters.
filterColsNoGaps :: MultiSequenceAlignment -> MultiSequenceAlignment
filterColsNoGaps msa = filterColsWith (V.all $ not . A.isGap (msa^.alphabet)) msa

-- | Frequency data; do not store the actual characters, but their frequencies.
-- The matrix is of size @N x K@, where @N@ is the number of sites, and @K@ is
-- the number of characters.
type FrequencyData = M.Matrix Double

-- | Calculcate frequency of characters at each site of a multi sequence alignment.
toFrequencyData :: MultiSequenceAlignment -> FrequencyData
toFrequencyData msa = fMapColParChunk 100 (D.frequencyCharacters spec) (msa^.matrix)
  where spec = A.alphabetSpec (msa^.alphabet)

-- | Calculate the distribution of characters.
distribution :: FrequencyData -> [Double]
distribution fd = map (/ fromIntegral nSites) $ V.toList $
  foldl1 (V.zipWith (+)) (M.toColumns fd)
  where nSites = M.cols fd

-- | Diversity analysis. See 'kEffEntropy'.
kEffEntropy :: FrequencyData -> [Double]
kEffEntropy fd = parMapChunk chunksize D.kEffEntropy (M.toColumns fd)

-- | Diversity analysis. See 'kEffEntropy'.
kEffHomoplasy :: FrequencyData -> [Double]
kEffHomoplasy fd = parMapChunk chunksize D.kEffHomoplasy (M.toColumns fd)

-- | Count the number of standard (i.e., not extended IUPAC) characters in the
-- alignment.
countIUPACChars :: MultiSequenceAlignment -> Int
countIUPACChars msa = V.length . V.filter (A.isIUPAC (msa^.alphabet)) $ allChars
  where allChars = M.flatten $ msa^.matrix

-- | Count the number of gaps in the alignment.
countGaps :: MultiSequenceAlignment -> Int
countGaps msa = V.length . V.filter (A.isGap (msa^.alphabet)) $ allChars
  where allChars = M.flatten $ msa^.matrix

-- | Count the number of unknown characters in the alignment.
countUnknowns :: MultiSequenceAlignment -> Int
countUnknowns msa = V.length . V.filter (A.isUnknown (msa^.alphabet)) $ allChars
  where allChars = M.flatten $ msa^.matrix

-- | Sample the given sites from a multi sequence alignment.
subSample :: [Int] -> MultiSequenceAlignment -> MultiSequenceAlignment
subSample is = over matrix (subSampleMatrix is)

-- | Randomly sample a given number of sites of the multi sequence alignment.
randomSubSample :: PrimMonad m
          => Int -> MultiSequenceAlignment  -> Gen (PrimState m) -> m MultiSequenceAlignment
randomSubSample n msa g = do
  let l = length msa
  is <- replicateM n $ uniformR (0, l-1) g
  return $ subSample is msa
