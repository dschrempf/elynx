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
  ( MultiSequenceAlignment (..)
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
  -- | * Analysis
  , diversityAnalysis
  ) where

import           Control.Monad
import           Data.Array.Repa                            as R
import qualified Data.ByteString.Lazy.Char8                 as B
import qualified Data.Vector.Unboxed                        as V
import           Data.Word8                                 (Word8)
import           Prelude                                    as P

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Alphabet.DistributionDiversity
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools.Equality
import           EvoMod.Tools.Repa
import           EvoMod.Tools.Vector

-- | A collection of sequences.
data MultiSequenceAlignment = MSA { msaNames :: [SequenceId]
                                  , msaCode  :: Code
                                  , msaData  :: Array U DIM2 Word8
                                  }

-- | Number of sites.
msaLength :: MultiSequenceAlignment -> Int
msaLength (MSA _ _ d) = nCols d

-- | Number of sequences.
msaNSequences :: MultiSequenceAlignment -> Int
msaNSequences (MSA _ _ d) = nRows d

-- | Create 'MultiSequenceAlignment' from a list of 'Sequence's.
fromSequenceList :: [Sequence] -> MultiSequenceAlignment
fromSequenceList ss
  | equalLength ss && allEqual (P.map seqCode ss) = MSA names code d
  | otherwise = error "Sequences do not have equal length."
  where
    names = P.map seqId ss
    code  = seqCode $ head ss
    len   = lengthSequence $ head ss
    nSeqs = length ss
    vs    = P.map (toUnboxed . seqCs) ss
    d     = fromUnboxed (ix2 nSeqs len) (V.concat vs)

-- | Conversion to list of 'Sequence's.
toSequenceList :: MultiSequenceAlignment -> [Sequence]
toSequenceList msa@(MSA names code d) = P.map
  (\n -> Sequence (names !! n) code (computeUnboxedS $ nThRow n d))
  [0..nSeqs-1]
  where
    nSeqs = msaNSequences msa

msaHeader :: B.ByteString
msaHeader = sequenceListHeader

-- | Show a 'MultiSequenceAlignment' in text form.
showMSA :: MultiSequenceAlignment -> B.ByteString
showMSA msa = msaHeader <> showSequenceList (toSequenceList msa)

-- | Similar to 'summarizeSequenceList' but with different Header.
summarizeMSA :: MultiSequenceAlignment -> B.ByteString
summarizeMSA msa = B.pack "Multi sequence alignment.\n" <> summarizeSequenceList (toSequenceList msa)

-- | Join two 'MultiSequenceAlignment's vertically. That is, add more sequences
-- to an alignment. See also 'msaConcatenate'.
msaJoin :: MultiSequenceAlignment
        -> MultiSequenceAlignment
        -> Either B.ByteString MultiSequenceAlignment
-- top bottom.
msaJoin t b
  | msaLength t == msaLength b &&
    msaCode t == msaCode b = Right $ MSA names (msaCode t) d
  | otherwise  = Left $ B.pack "msaJoin: Multi sequence alignments do not have equal length."
  where
    names = msaNames t P.++ msaNames b
    d     = computeUnboxedS $ msaData t R.++ msaData b

-- | Concatenate two 'MultiSequenceAlignment's horizontally. That is, add more
-- sites to an alignment. See also 'msaJoin'.
msaConcatenate :: MultiSequenceAlignment
               -> MultiSequenceAlignment
               -> Either B.ByteString MultiSequenceAlignment
-- left right.
msaConcatenate l r
  | msaNSequences l == msaNSequences r &&
    msaCode l == msaCode r = Right $ MSA (msaNames l) (msaCode l) d
  | otherwise = Left $ B.pack "msaConcatenate: Multi sequence alignments do not have equal length."
  where
    lD = msaData l
    rD = msaData r
    d = computeUnboxedS $ transpose $ transpose lD R.++ transpose rD

-- | Concatenate a list of 'MultiSequenceAlignment's horizontally. See
-- 'msaConcatenate'.
msasConcatenate :: [MultiSequenceAlignment] -> Either B.ByteString MultiSequenceAlignment
msasConcatenate []    = Left $ B.pack "Nothing to concatenate."
msasConcatenate [msa] = Right msa
msasConcatenate msas  = foldM msaConcatenate (head msas) (tail msas)

-- | Diversity analysis. See 'kEffEntropy'.
diversityAnalysis :: MultiSequenceAlignment -> B.ByteString
diversityAnalysis (MSA _ code d) = B.pack $ show $ meanVec $ fMapCol (kEffEntropy . frequencyCharacters code) d
