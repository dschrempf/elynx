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
  , FrequencyData
  , toFrequencyData
  , kEffAll
  , kEffMean
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8                 as L
import qualified Data.Matrix.Storable                       as M
import           Data.Word8                                 (Word8)

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Alphabet.DistributionDiversity
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools.Equality
import           EvoMod.Tools.Matrix
import           EvoMod.Tools.Misc

-- | A collection of sequences.
data MultiSequenceAlignment = MSA { msaNames :: [SequenceId]
                                  , msaCode  :: Code
                                  , msaData  :: M.Matrix Word8
                                  }

-- | Number of sites.
msaLength :: MultiSequenceAlignment -> Int
msaLength (MSA _ _ d) = M.cols d

-- | Number of sequences.
msaNSequences :: MultiSequenceAlignment -> Int
msaNSequences (MSA _ _ d) = M.rows d

-- | Create 'MultiSequenceAlignment' from a list of 'Sequence's.
fromSequenceList :: [Sequence] -> MultiSequenceAlignment
fromSequenceList ss
  | equalLength ss && allEqual (map seqCode ss) = MSA names code d
  | otherwise = error "Sequences do not have equal length."
  where
    names = map seqId ss
    code  = seqCode $ head ss
    vecs  = map seqCs ss
    d     = M.fromRows vecs

-- | Conversion to list of 'Sequence's.
toSequenceList :: MultiSequenceAlignment -> [Sequence]
toSequenceList (MSA ns c d) = zipWith (\n r -> Sequence n c r) ns rows
  where
    rows  = M.toRows d

msaHeader :: L.ByteString
msaHeader = sequenceListHeader

-- | Show a 'MultiSequenceAlignment' in text form.
showMSA :: MultiSequenceAlignment -> L.ByteString
showMSA msa = msaHeader <> showSequenceList (toSequenceList msa)

-- | Similar to 'summarizeSequenceList' but with different Header.
summarizeMSA :: MultiSequenceAlignment -> L.ByteString
summarizeMSA msa = L.pack "Multi sequence alignment.\n" <> summarizeSequenceList (toSequenceList msa)

-- | Join two 'MultiSequenceAlignment's vertically. That is, add more sequences
-- to an alignment. See also 'msaConcatenate'.
msaJoin :: MultiSequenceAlignment
        -> MultiSequenceAlignment
        -> Either L.ByteString MultiSequenceAlignment
-- top bottom.
msaJoin t b
  | msaLength t == msaLength b &&
    msaCode t == msaCode b = Right $ MSA names (msaCode t) (tD === bD)
  | otherwise  = Left $ L.pack "msaJoin: Multi sequence alignments do not have equal length."
  where
    names = msaNames t ++ msaNames b
    tD    = msaData t
    bD    = msaData b

-- | Concatenate two 'MultiSequenceAlignment's horizontally. That is, add more
-- sites to an alignment. See also 'msaJoin'.
msaConcatenate :: MultiSequenceAlignment
               -> MultiSequenceAlignment
               -> Either L.ByteString MultiSequenceAlignment
-- left right.
msaConcatenate l r
  | msaNSequences l == msaNSequences r &&
    msaCode l == msaCode r = Right $ MSA (msaNames l) (msaCode l) (lD ||| rD)
  | otherwise = Left $ L.pack "msaConcatenate: Multi sequence alignments do not have equal length."
  where
    lD = msaData l
    rD = msaData r

-- | Concatenate a list of 'MultiSequenceAlignment's horizontally. See
-- 'msaConcatenate'.
msasConcatenate :: [MultiSequenceAlignment] -> Either L.ByteString MultiSequenceAlignment
msasConcatenate []    = Left $ L.pack "Nothing to concatenate."
msasConcatenate [msa] = Right msa
msasConcatenate msas  = foldM msaConcatenate (head msas) (tail msas)

-- Convert alignment to frequency data.
type FrequencyData = M.Matrix Double

-- | Calculcate frequency of characters in multi sequence alignment.
toFrequencyData :: MultiSequenceAlignment -> FrequencyData
toFrequencyData (MSA _ c d) = fMapColParChunk 100 (frequencyCharacters c) d

-- | Diversity analysis. See 'kEffEntropy'.
kEffAll :: FrequencyData -> [Double]
kEffAll fd = parMapChunk 500 kEffEntropy (M.toColumns fd)

-- | Diversity analysis. See 'kEffEntropy'.
kEffMean :: FrequencyData -> Double
kEffMean = mean . kEffAll
  where mean xs = sum xs / fromIntegral (length xs)
