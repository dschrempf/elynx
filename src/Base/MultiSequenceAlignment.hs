{- |
Module      :  MultiSequenceAlignment
Description :  Multi sequence alignment related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:40:18 2018.

-}


module Base.MultiSequenceAlignment
  ( MultiSequenceAlignment (..)
  , showSummaryMSA
  , join
  ) where

import           Base.Alphabet
import           Base.Sequence

-- | A collection of names sequences with a specific length (i.e., the number of sites).
data MultiSequenceAlignment a = MSA { msaSequences  :: [NamedSequence a]
                                    , msaNSequences :: Int
                                    , msaLength     :: Int}

instance Show a => Show (MultiSequenceAlignment a) where
  show MSA{msaSequences=xs} = unlines $ (showSequenceName "Name" ++ "Sequence") : map show xs

showSummaryMSA :: (Show a, Alphabet a) => MultiSequenceAlignment a -> String
showSummaryMSA MSA{msaSequences=xs} = summarizeNamedSequenceListHeader "List" xs ++ "\n" ++ summarizeNamedSequenceListBody xs

join :: MultiSequenceAlignment a -> MultiSequenceAlignment a -> Maybe (MultiSequenceAlignment a)
join
  MSA{msaSequences=xs, msaNSequences=nex, msaLength=nix}
  MSA{msaSequences=ys, msaNSequences=ney, msaLength=niy}
  | nix == niy = Just $ MSA (xs ++ ys) (nex + ney) nix
  | otherwise  = Nothing
