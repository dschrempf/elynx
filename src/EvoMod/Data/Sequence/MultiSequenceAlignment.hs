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
  ) where

import           Control.Monad
import           Data.Array.Repa               as R
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.Vector.Unboxed           as V
import           Data.Word8                    (Word8)
import           Prelude                       as P

import           EvoMod.Data.Sequence.Sequence

-- | A collection of sequences.
data MultiSequenceAlignment = MSA { msaNames :: [SequenceId]
                                  , msaData  :: Array U DIM2 Word8
                                  }

-- | Number of sites.
msaLength :: MultiSequenceAlignment -> Int
msaLength (MSA _ d) = listOfShape (extent d) !! 1

-- | Number of sequences.
msaNSequences :: MultiSequenceAlignment -> Int
msaNSequences (MSA _ d) = head $ listOfShape (extent d)

-- | Create 'MultiSequenceAlignment' from a list of 'Sequence's.
fromSequenceList :: [Sequence] -> MultiSequenceAlignment
fromSequenceList ss | equalLength ss = MSA names d
                    | otherwise      = error "Sequences do not have equal length."
                    where
                      names = P.map seqId ss
                      len   = lengthSequence $ head ss
                      nSeqs = length ss
                      vs    = P.map (toUnboxed . seqCs) ss
                      d     = fromUnboxed (ix2 len nSeqs) (V.concat vs)

toSequenceList :: MultiSequenceAlignment -> [Sequence]
toSequenceList msa@(MSA names d) = P.map
  (\n -> Sequence (names !! n) (computeUnboxedS $ slice d (Z :. n :. All)))
  [0..nSeqs]
  where
    nSeqs = msaNSequences msa

msaHeader :: MultiSequenceAlignment -> B.ByteString
msaHeader msa = B.unlines
    [ B.pack "Multi sequence alignment."
    , B.pack $ "Length: " P.++ show (msaLength msa) P.++ "."
    , sequenceListHeader ]

-- | Show a 'MultiSequenceAlignment' in text form.
showMSA :: MultiSequenceAlignment -> B.ByteString
showMSA msa = msaHeader msa <> showSequenceList (toSequenceList msa)

-- instance Show MultiSequenceAlignment where
--   show msa = msaHeader msa ++ showSequenceList (msaSequences msa)

-- | Similar to 'summarizeSequenceList' but with different Header.
summarizeMSA :: MultiSequenceAlignment -> B.ByteString
summarizeMSA msa = msaHeader msa <> summarizeSequenceListBody (toSequenceList msa)

-- | Join two 'MultiSequenceAlignment's vertically. That is, add more sequences
-- to an alignment. See also 'msaConcatenate'.
msaJoin :: MultiSequenceAlignment
        -> MultiSequenceAlignment
        -> Either B.ByteString MultiSequenceAlignment
-- top bottom.
msaJoin t b
  | msaLength t == msaLength b = Right $ MSA names d
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
  | msaNSequences l == msaNSequences r = Right $ MSA (msaNames l) d
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
