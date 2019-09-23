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

-}


module ELynx.Data.Sequence.MultiSequenceAlignment
  ( MultiSequenceAlignment (MultiSequenceAlignment)
  , msaAlphabet
  , msaLength
  , msaNSequences
  -- | * Input, output
  , fromSequenceList
  , toSequenceList
  , showMSA
  , summarizeMSA
  -- | * Manipulation
  , msaJoin
  , msaConcatenate
  , msasConcatenate
  , filterColumnsOnlyStd
  , filterColumnsStd
  , filterColumnsNoGaps
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
import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.ByteString.Lazy.Char8                as L
import           Data.List
import qualified Data.Matrix.Unboxed                       as M
import qualified Data.Vector.Unboxed                       as V
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
                              { _names    :: [S.SequenceName]
                              , _alphabet :: A.Alphabet
                              , _matrix   :: M.Matrix Character
                              }
  deriving (Read, Show, Eq)

makeLenses ''MultiSequenceAlignment

-- | Alphabet.
msaAlphabet :: MultiSequenceAlignment -> A.Alphabet
msaAlphabet = view alphabet

-- | Number of sites.
msaLength :: MultiSequenceAlignment -> Int
msaLength = M.cols . view matrix

-- | Number of sequences.
msaNSequences :: MultiSequenceAlignment -> Int
msaNSequences = M.rows . view matrix

-- | Create 'MultiSequenceAlignment' from a list of 'S.Sequence's.
fromSequenceList :: [S.Sequence] -> Either String MultiSequenceAlignment
fromSequenceList ss
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
toSequenceList :: MultiSequenceAlignment -> [S.Sequence]
toSequenceList (MultiSequenceAlignment ns a d) = zipWith (\n r -> S.Sequence n a r) ns rows
  where
    rows  = M.toRows d

msaHeader :: L.ByteString
msaHeader = L.unwords [ alignLeft defSequenceNameWidth (L.pack "Name")
                      , L.pack "Sequence" ]

-- | Show a 'S.Sequence', untrimmed.
showSequence :: MultiSequenceAlignment -> Int -> L.ByteString
showSequence m i =
  L.unwords [ alignLeft defSequenceNameWidth $ (m ^. names) !! i
            , S.fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'S.Sequence', untrimmed.
summarizeSequence :: MultiSequenceAlignment -> Int -> L.ByteString
summarizeSequence m i =
  L.unwords [ alignLeft defSequenceNameWidth $ (m ^. names) !! i
            , summarizeByteString defSequenceSummaryLength $
              S.fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'MultiSequenceAlignment' in text form.
showMSA :: MultiSequenceAlignment -> L.ByteString
showMSA msa = L.unlines $ msaHeader :
  map (showSequence msa) [0 .. (msaNSequences msa - 1)]

summarizeMSAHeader :: MultiSequenceAlignment -> L.ByteString
summarizeMSAHeader msa = L.unlines $
  [ L.pack "Multi sequence alignment."
  , L.pack $ "Code: " ++ A.alphabetNameVerbose (msa^.alphabet) ++ "."
  , L.pack $ "Length: " ++ show (msaLength msa) ++ "." ]
  ++ reportLengthSummary ++ reportNumberSummary
  where reportLengthSummary =
          [ L.pack $ "For each sequence, the "
            ++ show defSequenceSummaryLength ++ " first bases are shown."
          | msaLength msa > defSequenceSummaryLength ]
        reportNumberSummary =
          [ L.pack $ show defSequenceListSummaryNumber ++ " out of " ++
            show (msaNSequences msa) ++ " sequences are shown."
          | msaNSequences msa > defSequenceListSummaryNumber ]

-- | Similar to 'S.summarizeSequenceList' but with different Header.
summarizeMSA :: MultiSequenceAlignment -> L.ByteString
summarizeMSA msa = L.unlines $ summarizeMSAHeader msa :
  map (summarizeSequence msa) [0 .. n - 1]
  where n = min (msaNSequences msa) defSequenceListSummaryNumber

-- | Join two 'MultiSequenceAlignment's vertically. That is, add more sequences
-- to an alignment. See also 'msaConcatenate'.
msaJoin :: MultiSequenceAlignment
        -> MultiSequenceAlignment
        -> MultiSequenceAlignment
-- top bottom.
msaJoin t b
  | msaLength t == msaLength b
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
msaConcatenate :: MultiSequenceAlignment
               -> MultiSequenceAlignment
               -> MultiSequenceAlignment
-- left right.
msaConcatenate l r
  | msaNSequences l == msaNSequences r
    && l^.alphabet == r^.alphabet
  = MultiSequenceAlignment (l ^. names) a (lD ||| rD)
  | otherwise = error "msaConcatenate: Multi sequence alignments do not have equal length."
  where
    lD = l^.matrix
    rD = r^.matrix
    a  = l^.alphabet

-- | Concatenate a list of 'MultiSequenceAlignment's horizontally. See
-- 'msaConcatenate'.
msasConcatenate :: [MultiSequenceAlignment] -> MultiSequenceAlignment
msasConcatenate []    = error "msasConcatenate: Nothing to concatenate."
msasConcatenate [msa] = msa
msasConcatenate msas  = foldl' msaConcatenate (head msas) (tail msas)

-- Only keep columns from alignment that satisfy given predicate.
filterColumnsWith :: (V.Vector Character -> Bool) -> MultiSequenceAlignment -> MultiSequenceAlignment
filterColumnsWith p = over matrix (M.fromColumns . filter p . M.toColumns)

-- | Only keep columns with standard characters. Alignment columns with IUPAC
-- characters are removed.
filterColumnsOnlyStd :: MultiSequenceAlignment -> MultiSequenceAlignment
filterColumnsOnlyStd msa = filterColumnsWith (V.all $ A.isStd (msa^.alphabet)) msa

-- | Filter columns with proportion of standard character larger than given number.
filterColumnsStd :: Double -> MultiSequenceAlignment -> MultiSequenceAlignment
filterColumnsStd prop msa = filterColumnsWith
  (\col -> prop * nSeqs <= fromIntegral (V.length (V.filter (A.isStd a) col)))
  msa
  where a = msa^.alphabet
        nSeqs = fromIntegral $ msaNSequences msa

-- | Only keep columns without gaps or unknown characters.
filterColumnsNoGaps :: MultiSequenceAlignment -> MultiSequenceAlignment
filterColumnsNoGaps msa = filterColumnsWith (V.all $ not . A.isGap (msa^.alphabet)) msa

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
  let l = msaLength msa
  is <- replicateM n $ uniformR (0, l-1) g
  return $ subSample is msa
