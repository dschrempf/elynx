{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  EvoMod.Data.MultiSequenceAlignment
Description :  Multi sequence alignment related types and functions.
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
  , filterColumnsGapsUnknowns
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
import qualified Data.Matrix.Unboxed                        as M
import qualified Data.Vector.Unboxed                        as V
import           System.Random.MWC

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Alphabet.DistributionDiversity
import           EvoMod.Data.Sequence.Defaults
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Concurrent
import           EvoMod.Tools.Equality
import           EvoMod.Tools.Matrix

-- | A collection of sequences.
data MultiSequenceAlignment = MultiSequenceAlignment
  { _names  :: [SequenceName]
  , _code   :: Code
  , _matrix :: M.Matrix Character
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
fromSequenceList :: [Sequence] -> MultiSequenceAlignment
fromSequenceList ss
  | equalLength ss && allEqual (map (view seqCode) ss) = MultiSequenceAlignment ns cd d
  | otherwise = error "Sequences do not have equal length or equal codes."
  where
    ns   = map (view seqName) ss
    cd   = head ss ^. seqCode
    bss  = map (view seqCharacters) ss
    d    = M.fromRows bss

-- | Conversion to list of 'Sequence's.
toSequenceList :: MultiSequenceAlignment -> [Sequence]
toSequenceList (MultiSequenceAlignment ns c d) = zipWith (\n r -> Sequence n c r) ns rows
  where
    rows  = M.toRows d

msaHeader :: L.ByteString
msaHeader = L.unwords [ alignLeft defSequenceNameWidth (L.pack "Name")
                      , L.pack "Sequence" ]

-- | Show a 'Sequence', untrimmed.
showSequenceOfMultiSequenceAlignment :: MultiSequenceAlignment -> Int -> L.ByteString
showSequenceOfMultiSequenceAlignment m i =
  L.unwords [ alignLeft defSequenceNameWidth $ (m ^. names) !! i
            , fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'Sequence', untrimmed.
summarizeSequenceOfMultiSequenceAlignment :: MultiSequenceAlignment -> Int -> L.ByteString
summarizeSequenceOfMultiSequenceAlignment m i =
  L.unwords [ alignLeft defSequenceNameWidth $ (m ^. names) !! i
            , summarizeByteString defSequenceSummaryLength $
              fromCharacters $ M.takeRow (m ^. matrix) i ]

-- | Show a 'MultiSequenceAlignment' in text form.
showMSA :: MultiSequenceAlignment -> L.ByteString
showMSA msa = L.unlines $ msaHeader :
  map (showSequenceOfMultiSequenceAlignment msa) [0 .. (msaNSequences msa - 1)]

summarizeMSAHeader :: MultiSequenceAlignment -> L.ByteString
summarizeMSAHeader msa = L.unlines $
  [ L.pack "Multi sequence alignment."
  , L.pack $ "Code: " ++ codeNameVerbose (msa ^. code) ++ "."
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
        -> Either L.ByteString MultiSequenceAlignment
-- top bottom.
msaJoin t b
  | msaLength t == msaLength b &&
    t ^. code   == b ^. code = Right $ MultiSequenceAlignment ns (t ^. code) (tD === bD)
  | otherwise  = Left $ L.pack "msaJoin: Multi sequence alignments do not have equal length."
  where
    ns = t ^. names ++ b ^. names
    tD = t ^. matrix
    bD = t ^. matrix

-- | Concatenate two 'MultiSequenceAlignment's horizontally. That is, add more
-- sites to an alignment. See also 'msaJoin'.
msaConcatenate :: MultiSequenceAlignment
               -> MultiSequenceAlignment
               -> Either L.ByteString MultiSequenceAlignment
-- left right.
msaConcatenate l r
  | msaNSequences l == msaNSequences r &&
    l ^. code == r ^. code = Right $ MultiSequenceAlignment (l ^. names) (l ^. code) (lD ||| rD)
  | otherwise = Left $ L.pack "msaConcatenate: Multi sequence alignments do not have equal length."
  where
    lD = l ^. matrix
    rD = r ^. matrix

-- | Concatenate a list of 'MultiSequenceAlignment's horizontally. See
-- 'msaConcatenate'.
msasConcatenate :: [MultiSequenceAlignment] -> Either L.ByteString MultiSequenceAlignment
msasConcatenate []    = Left $ L.pack "Nothing to concatenate."
msasConcatenate [msa] = Right msa
msasConcatenate msas  = foldM msaConcatenate (head msas) (tail msas)

-- Only keep columns from alignment that satisfy given predicate.
filterColumns :: (V.Vector Character -> Bool) -> MultiSequenceAlignment -> MultiSequenceAlignment
filterColumns p = over matrix (M.fromColumns . filter p . M.toColumns)

-- | Only keep columns with standard characters. Alignment columns with IUPAC
-- characters are removed.
filterColumnsIUPAC :: MultiSequenceAlignment -> MultiSequenceAlignment
filterColumnsIUPAC msa = filterColumns (V.all (isStandard (msa^.code))) msa

-- | Only keep columns without gaps or unknown characters.
filterColumnsGapsUnknowns :: MultiSequenceAlignment -> MultiSequenceAlignment
filterColumnsGapsUnknowns msa = filterColumns (V.all (isGapOrUnknown (msa^.code))) msa

-- | Frequency data; do not store the actual characters, but only their
-- frequencies.
type FrequencyData = M.Matrix Double

-- | Calculcate frequency of characters in multi sequence alignment.
toFrequencyData :: MultiSequenceAlignment -> FrequencyData
toFrequencyData (MultiSequenceAlignment _ c d) = fMapColParChunk 100 (frequencyCharacters c) d

-- | Diversity analysis. See 'kEffEntropy'.
kEff :: FrequencyData -> [Double]
kEff fd = parMapChunk 500 kEffEntropy (M.toColumns fd)

-- | Count the number of standard (i.e., not extended IUPAC) characters in the
-- alignment.
countStandardChars :: MultiSequenceAlignment -> Int
countStandardChars msa = V.length . V.filter (isStandard cd) $ allChars
  where allChars = M.flatten $ msa^.matrix
        cd       = msa^.code

-- | Count the number of gaps or unknown characters in the alignment.
countGapOrUnknownChars :: MultiSequenceAlignment -> Int
countGapOrUnknownChars msa = V.length . V.filter (isGapOrUnknown cd) $ allChars
  where allChars = M.flatten $ msa^.matrix
        cd       = msa^.code

-- | Sample the given sites from a multi sequence alignment.
subSample :: [Int] -> MultiSequenceAlignment -> MultiSequenceAlignment
subSample is = over matrix (subSampleMatrix is)

-- | Randomly sample a given number of sites of the multi sequence alignment.
randomSubSample :: (PrimMonad m)
          => Int -> MultiSequenceAlignment  -> Gen (PrimState m) -> m MultiSequenceAlignment
randomSubSample n msa g = do
  let l = msaLength msa
  is <- replicateM n $ uniformR (0, l-1) g
  return $ subSample is msa
