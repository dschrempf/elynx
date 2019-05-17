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
  , filterColumnsIUPAC
  , filterColumnsGaps
  -- | * Analysis
  , FrequencyData
  , toFrequencyData
  , kEff
  , countStandardChars
  , countGapOrUnknownChars
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

import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Alphabet.DistributionDiversity
import           EvoMod.Data.Sequence.Defaults
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Concurrent
import           EvoMod.Tools.Matrix

-- | A collection of sequences.
data MultiSequenceAlignment a = MultiSequenceAlignment
  { _names  :: [SequenceName]
  , _matrix :: M.Matrix a
  }
  deriving (Read, Show, Eq)

makeLenses ''MultiSequenceAlignment

-- | Number of sites.
msaLength :: Character a => MultiSequenceAlignment a -> Int
msaLength = M.cols . view matrix

-- | Number of sequences.
msaNSequences :: Character a => MultiSequenceAlignment a -> Int
msaNSequences = M.rows . view matrix

-- | Create 'MultiSequenceAlignment' from a list of 'Sequence's.
fromSequenceList :: Character a => [Sequence a] -> MultiSequenceAlignment a
fromSequenceList ss
  | equalLength ss = MultiSequenceAlignment ns d
  | otherwise = error "Sequences do not have equal length or equal codes."
  where
    ns   = map (view name) ss
    bss  = map (view characters) ss
    d    = M.fromRows bss

-- | Conversion to list of 'Sequence's.
toSequenceList :: Character a => MultiSequenceAlignment a -> [Sequence a]
toSequenceList (MultiSequenceAlignment ns d) = zipWith Sequence ns rows
  where
    rows  = M.toRows d

msaHeader :: L.ByteString
msaHeader = L.unwords [ alignLeft defSequenceNameWidth (L.pack "Name")
                      , L.pack "Sequence" ]

-- | Show a 'Sequence', untrimmed.
showSequenceOfMultiSequenceAlignment :: Character a => MultiSequenceAlignment a -> Int -> L.ByteString
showSequenceOfMultiSequenceAlignment m i =
  L.unwords [ alignLeft defSequenceNameWidth $ (m ^. names) !! i
            , fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'Sequence', untrimmed.
summarizeSequenceOfMultiSequenceAlignment :: Character a
                                          => MultiSequenceAlignment a -> Int -> L.ByteString
summarizeSequenceOfMultiSequenceAlignment m i =
  L.unwords [ alignLeft defSequenceNameWidth $ (m ^. names) !! i
            , summarizeByteString defSequenceSummaryLength $
              fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'MultiSequenceAlignment' in text form.
showMSA :: Character a => MultiSequenceAlignment a -> L.ByteString
showMSA msa = L.unlines $ msaHeader :
  map (showSequenceOfMultiSequenceAlignment msa) [0 .. (msaNSequences msa - 1)]

summarizeMSAHeader :: forall a . Character a => MultiSequenceAlignment a -> L.ByteString
summarizeMSAHeader msa = L.unlines $
  [ L.pack "Multi sequence alignment."
  , L.pack $ "Code: " ++ codeNameVerbose (code @a) ++ "."
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
summarizeMSA :: Character a => MultiSequenceAlignment a -> L.ByteString
summarizeMSA msa = L.unlines $ summarizeMSAHeader msa :
  map (summarizeSequenceOfMultiSequenceAlignment msa) [0 .. n - 1]
  where n = min (msaNSequences msa) defSequenceListSummaryNumber

-- | Join two 'MultiSequenceAlignment's vertically. That is, add more sequences
-- to an alignment. See also 'msaConcatenate'.
msaJoin :: Character a
        => MultiSequenceAlignment a
        -> MultiSequenceAlignment a
        -> MultiSequenceAlignment a
-- top bottom.
msaJoin t b
  | msaLength t == msaLength b = MultiSequenceAlignment ns (tD === bD)
  | otherwise  = error "msaJoin: Multi sequence alignments do not have equal length."
  where
    ns = t ^. names ++ b ^. names
    tD = t ^. matrix
    bD = t ^. matrix

-- | Concatenate two 'MultiSequenceAlignment's horizontally. That is, add more
-- sites to an alignment. See also 'msaJoin'.
msaConcatenate :: Character a
               => MultiSequenceAlignment a
               -> MultiSequenceAlignment a
               -> MultiSequenceAlignment a
-- left right.
msaConcatenate l r
  | msaNSequences l == msaNSequences r = MultiSequenceAlignment (l ^. names) (lD ||| rD)
  | otherwise = error "msaConcatenate: Multi sequence alignments do not have equal length."
  where
    lD = l ^. matrix
    rD = r ^. matrix

-- | Concatenate a list of 'MultiSequenceAlignment's horizontally. See
-- 'msaConcatenate'.
msasConcatenate :: Character a => [MultiSequenceAlignment a] -> MultiSequenceAlignment a
msasConcatenate []    = error "msasConcatenate: Nothing to concatenate."
msasConcatenate [msa] = msa
msasConcatenate msas  = foldl' msaConcatenate (head msas) (tail msas)

-- Only keep columns from alignment that satisfy given predicate.
filterColumns :: Character a
              => (V.Vector a -> Bool) -> MultiSequenceAlignment a -> MultiSequenceAlignment a
filterColumns p = over matrix (M.fromColumns . filter p . M.toColumns)

-- | Only keep columns with standard characters. Alignment columns with IUPAC
-- characters are removed.
filterColumnsIUPAC :: CharacterI a => MultiSequenceAlignment a -> MultiSequenceAlignment a
filterColumnsIUPAC = filterColumns (V.all isStandard)

-- | Only keep columns without gaps or unknown characters.
filterColumnsGaps :: CharacterX a => MultiSequenceAlignment a -> MultiSequenceAlignment a
filterColumnsGaps = filterColumns (V.all isGap)

-- | Frequency data; do not store the actual characters, but only their
-- frequencies.
type FrequencyData = M.Matrix Double

-- | Calculcate frequency of characters in multi sequence alignment.
toFrequencyData :: CharacterI a => MultiSequenceAlignment a -> FrequencyData
toFrequencyData (MultiSequenceAlignment _ d) = fMapColParChunk 100 frequencyCharacters d

-- | Diversity analysis. See 'kEffEntropy'.
kEff :: FrequencyData -> [Double]
kEff fd = parMapChunk 500 kEffEntropy (M.toColumns fd)

-- | Count the number of standard (i.e., not extended IUPAC) characters in the
-- alignment.
countStandardChars :: CharacterI a => MultiSequenceAlignment a -> Int
countStandardChars msa = V.length . V.filter isStandard $ allChars
  where allChars = M.flatten $ msa^.matrix

-- | Count the number of gaps or unknown characters in the alignment.
countGapOrUnknownChars :: CharacterX a => MultiSequenceAlignment a -> Int
countGapOrUnknownChars msa = V.length . V.filter isGap $ allChars
  where allChars = M.flatten $ msa^.matrix

-- | Sample the given sites from a multi sequence alignment.
subSample :: Character a => [Int] -> MultiSequenceAlignment a -> MultiSequenceAlignment a
subSample is = over matrix (subSampleMatrix is)

-- | Randomly sample a given number of sites of the multi sequence alignment.
randomSubSample :: (PrimMonad m, Character a)
          => Int -> MultiSequenceAlignment a  -> Gen (PrimState m) -> m (MultiSequenceAlignment a)
randomSubSample n msa g = do
  let l = msaLength msa
  is <- replicateM n $ uniformR (0, l-1) g
  return $ subSample is msa
