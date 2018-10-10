{- |
Module      :  Evol.Data.MultiSequenceAlignment
Description :  Multi sequence alignment related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:40:18 2018.

-}


module Evol.Data.MultiSequenceAlignment
  ( MultiSequenceAlignment (..)
  , summarizeMSA
  , join
  ) where

import qualified Data.Vector.Unboxed as V

import           Evol.Data.Alphabet
import           Evol.Data.Sequence

-- | A collection of names sequences with a specific length (i.e., the number of sites).
data MultiSequenceAlignment i a = MSA { msaSequences  :: [Sequence i a]
                                      , msaNSequences :: Int
                                      , msaLength     :: Int}

instance (Show i, Show a, V.Unbox a) => Show (MultiSequenceAlignment i a) where
  show MSA{msaSequences=xs} = unlines $ (showSequenceId "Name" ++ "Sequence") : map show xs

summarizeMSA :: (Show i, Show a, Character a, V.Unbox a) => MultiSequenceAlignment i a -> String
summarizeMSA MSA{msaSequences=xs} = summarizeSequenceListHeader "List" xs ++ summarizeSequenceListBody xs

join :: MultiSequenceAlignment i a -> MultiSequenceAlignment i a -> Maybe (MultiSequenceAlignment i a)
join
  MSA{msaSequences=xs, msaNSequences=nex, msaLength=nix}
  MSA{msaSequences=ys, msaNSequences=ney, msaLength=niy}
  | nix == niy = Just $ MSA (xs ++ ys) (nex + ney) nix
  | otherwise  = Nothing
