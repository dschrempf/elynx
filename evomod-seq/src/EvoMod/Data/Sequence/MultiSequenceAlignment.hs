{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{- |
Module      :  EvoMod.Data.MultiSequenceAlignment
Description :  Multi sequence alignment related types and functions
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable

Portability :  portable

Creation date: Thu Oct  4 18:40:18 2018.

-}


module EvoMod.Data.Sequence.MultiSequenceAlignment
  ( MultiSequenceAlignment (MultiSequenceAlignment)
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
  , filterColumnsNoGaps
  -- | * Analysis
  , FrequencyData
  , toFrequencyData
  , kEff
  , countIUPACChars
  , countGaps
  -- | * Sub sample
  , subSample
  , randomSubSample
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.ByteString.Lazy.Char8                 as L
import           Data.List
import qualified Data.Matrix.Unboxed                        as M
import qualified Data.Vector.Unboxed                        as V
import           System.Random.MWC

import qualified EvoMod.Data.Alphabet.Alphabet              as A
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Alphabet.DistributionDiversity
import           EvoMod.Data.Sequence.Defaults
import qualified EvoMod.Data.Sequence.Sequence              as S
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Concurrent
import           EvoMod.Tools.Equality
import           EvoMod.Tools.Matrix

-- | A collection of sequences.
data MultiSequenceAlignment = MultiSequenceAlignment
                              { _names    :: [S.SequenceName]
                              , _alphName :: A.AlphabetName
                              , _matrix   :: M.Matrix Character
                              }
  deriving (Read, Show, Eq)

makeLenses ''MultiSequenceAlignment

-- | Number of sites.
msaLength :: MultiSequenceAlignment -> Int
msaLength = M.cols . view matrix

-- | Number of sequences.
msaNSequences :: MultiSequenceAlignment -> Int
msaNSequences = M.rows . view matrix

-- | Create 'MultiSequenceAlignment' from a list of 'Sequence's.
fromSequenceList :: [S.Sequence] -> MultiSequenceAlignment
fromSequenceList ss
  | S.equalLength ss && allEqual (map (view S.alphName) ss) = MultiSequenceAlignment ns a d
  | otherwise = error "Sequences do not have equal length or equal codes."
  where
    ns   = map (view S.name) ss
    a    = head ss ^. S.alphName
    bss  = map (view S.characters) ss
    d    = M.fromRows bss

-- | Conversion to list of 'Sequence's.
toSequenceList :: MultiSequenceAlignment -> [S.Sequence]
toSequenceList (MultiSequenceAlignment ns a d) = zipWith (\n r -> S.Sequence n a r) ns rows
  where
    rows  = M.toRows d

msaHeader :: L.ByteString
msaHeader = L.unwords [ alignLeft defSequenceNameWidth (L.pack "Name")
                      , L.pack "Sequence" ]

-- | Show a 'Sequence', untrimmed.
showSequenceOfMultiSequenceAlignment :: MultiSequenceAlignment -> Int -> L.ByteString
showSequenceOfMultiSequenceAlignment m i =
  L.unwords [ alignLeft defSequenceNameWidth $ (m ^. names) !! i
            , S.fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'Sequence', untrimmed.
summarizeSequenceOfMultiSequenceAlignment :: MultiSequenceAlignment -> Int -> L.ByteString
summarizeSequenceOfMultiSequenceAlignment m i =
  L.unwords [ alignLeft defSequenceNameWidth $ (m ^. names) !! i
            , summarizeByteString defSequenceSummaryLength $
              S.fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'MultiSequenceAlignment' in text form.
showMSA :: MultiSequenceAlignment -> L.ByteString
showMSA msa = L.unlines $ msaHeader :
  map (showSequenceOfMultiSequenceAlignment msa) [0 .. (msaNSequences msa - 1)]

summarizeMSAHeader :: MultiSequenceAlignment -> L.ByteString
summarizeMSAHeader msa = L.unlines $
  [ L.pack "Multi sequence alignment."
  , L.pack $ "Code: " ++ A.alphabetNameVerbose (msa^.alphName) ++ "."
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

-- | Similar to 'summarizeSequenceList' but with different Header.
summarizeMSA :: MultiSequenceAlignment -> L.ByteString
summarizeMSA msa = L.unlines $ summarizeMSAHeader msa :
  map (summarizeSequenceOfMultiSequenceAlignment msa) [0 .. n - 1]
  where n = min (msaNSequences msa) defSequenceListSummaryNumber

-- | Join two 'MultiSequenceAlignment's vertically. That is, add more sequences
-- to an alignment. See also 'msaConcatenate'.
msaJoin :: MultiSequenceAlignment
        -> MultiSequenceAlignment
        -> MultiSequenceAlignment
-- top bottom.
msaJoin t b
  | msaLength t == msaLength b
    && t^.alphName == b^.alphName
  = MultiSequenceAlignment ns a (tD === bD)
  | otherwise  = error "msaJoin: Multi sequence alignments do not have equal length."
  where
    ns = t^.names ++ b^.names
    tD = t^.matrix
    bD = t^.matrix
    a  = t^.alphName

-- | Concatenate two 'MultiSequenceAlignment's horizontally. That is, add more
-- sites to an alignment. See also 'msaJoin'.
msaConcatenate :: MultiSequenceAlignment
               -> MultiSequenceAlignment
               -> MultiSequenceAlignment
-- left right.
msaConcatenate l r
  | msaNSequences l == msaNSequences r
    && l^.alphName == r^.alphName
  = MultiSequenceAlignment (l ^. names) a (lD ||| rD)
  | otherwise = error "msaConcatenate: Multi sequence alignments do not have equal length."
  where
    lD = l^.matrix
    rD = r^.matrix
    a  = l^.alphName

-- | Concatenate a list of 'MultiSequenceAlignment's horizontally. See
-- 'msaConcatenate'.
msasConcatenate :: [MultiSequenceAlignment] -> MultiSequenceAlignment
msasConcatenate []    = error "msasConcatenate: Nothing to concatenate."
msasConcatenate [msa] = msa
msasConcatenate msas  = foldl' msaConcatenate (head msas) (tail msas)

-- Only keep columns from alignment that satisfy given predicate.
filterColumns :: (V.Vector Character -> Bool) -> MultiSequenceAlignment -> MultiSequenceAlignment
filterColumns p = over matrix (M.fromColumns . filter p . M.toColumns)

-- | Only keep columns with standard characters. Alignment columns with IUPAC
-- characters are removed.
filterColumnsOnlyStd :: MultiSequenceAlignment -> MultiSequenceAlignment
filterColumnsOnlyStd msa = filterColumns (V.all $ A.isStd (msa^.alphName)) msa

-- | Only keep columns without gaps or unknown characters.
filterColumnsNoGaps :: MultiSequenceAlignment -> MultiSequenceAlignment
filterColumnsNoGaps msa = filterColumns (V.all $ not . A.isGap (msa^.alphName)) msa

-- | Frequency data; do not store the actual characters, but only their
-- frequencies.
type FrequencyData = M.Matrix Double

-- | Calculcate frequency of characters in multi sequence alignment.
toFrequencyData :: MultiSequenceAlignment -> FrequencyData
toFrequencyData msa = fMapColParChunk 100 (frequencyCharacters spec) (msa^.matrix)
  where spec = A.alphabetSpec (msa^.alphName)

-- | Diversity analysis. See 'kEffEntropy'.
kEff :: FrequencyData -> [Double]
kEff fd = parMapChunk 500 kEffEntropy (M.toColumns fd)

-- | Count the number of standard (i.e., not extended IUPAC) characters in the
-- alignment.
countIUPACChars :: MultiSequenceAlignment -> Int
countIUPACChars msa = V.length . V.filter (A.isIUPAC (msa^.alphName)) $ allChars
  where allChars = M.flatten $ msa^.matrix

-- | Count the number of gaps or unknown characters in the alignment.
countGaps :: MultiSequenceAlignment -> Int
countGaps msa = V.length . V.filter (A.isGap (msa^.alphName)) $ allChars
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
