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
  -- | * Input
  , fromSequenceList
  -- | * Output
  , showMSA
  , summarizeMSA
  -- | * Analysis
  , msaNSequences
  -- | * Manipulation
  , msaJoin
  , msaConcatenate
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as B

import           EvoMod.Data.Sequence.Sequence

-- | A collection of names sequences with a specific length (i.e., the number of sites).
data MultiSequenceAlignment = MSA { msaSequences :: [Sequence]
                                  , msaLength    :: Int}

-- | Create 'MultiSequenceAlignment' from a list of 'Sequence's.
fromSequenceList :: [Sequence] -> MultiSequenceAlignment
fromSequenceList ss | equalLength ss = MSA ss (lengthSequence $ head ss)
                    | otherwise      = error "Sequences do not have equal length."

msaHeader :: MultiSequenceAlignment -> B.ByteString
msaHeader (MSA _ l) = B.unlines
    [ B.pack "Multi sequence alignment."
    , B.pack $ "Length: " ++ show l ++ "."
    , sequenceListHeader ]

-- | Show a 'MultiSequenceAlignment' in text form.
showMSA :: MultiSequenceAlignment -> B.ByteString
showMSA msa = msaHeader msa <> showSequenceList (msaSequences msa)

-- instance Show MultiSequenceAlignment where
--   show msa = msaHeader msa ++ showSequenceList (msaSequences msa)

-- | Similar to 'summarizeSequenceList' but with different Header.
summarizeMSA :: MultiSequenceAlignment -> B.ByteString
summarizeMSA msa = msaHeader msa <> summarizeSequenceListBody (msaSequences msa)

-- | Number of sequences stored in 'MultiSequenceAlignment'.
msaNSequences :: MultiSequenceAlignment -> Int
msaNSequences = length . msaSequences

-- | Join two 'MultiSequenceAlignment's vertically. That is, add more sequences
-- to an alignment. See also 'msaConcatenate'.
msaJoin :: MultiSequenceAlignment
        -> MultiSequenceAlignment
        -> Either String MultiSequenceAlignment
msaJoin (MSA xs lx) (MSA ys ly)
  | lx == ly = Right $ MSA (xs ++ ys) lx
  | otherwise  = Left "msaJoin: Multi sequence alignments do not have equal length."

-- | Concatenate two 'MultiSequenceAlignment's horizontally. That is, add more
-- sites to an alignment. See also 'msaJoin'.
msaConcatenate :: MultiSequenceAlignment
               -> MultiSequenceAlignment
               -> Either String MultiSequenceAlignment
msaConcatenate (MSA xs lx) (MSA ys ly)
  | lx /= ly =
    Left "msaConcatenate: Multi sequence alignments do not have equal length."
  | otherwise = fromSequenceList <$> zipWithM concatenate xs ys
