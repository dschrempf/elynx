-- |
-- Module      :  Tree
-- Description :  Benchmark some tree algorithms
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Jul  2 13:10:48 2021.
module Tree
  ( toLengthTreeTraversable,
    toLengthTreeBitraversable,
  )
where

import Data.Bitraversable
import Data.Maybe
import ELynx.Tree.Length
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted

-- Compare speed of bitraversal and special traversal instances.

cleanStemLength :: HasMaybeLength e => Tree e a -> Tree e a
cleanStemLength = modifyStem f
  where
    f x = case getMaybeLength x of
      Nothing -> setMaybeLength 0 x
      Just _ -> x

toLengthTreeTraversable :: Tree Phylo a -> Tree Length a
toLengthTreeTraversable = either error id . toLengthTree

toLengthTreeBitraversable :: Tree Phylo a -> Tree Length a
toLengthTreeBitraversable t =
  fromMaybe
    (error "toLengthTree: Length unavailable for some branches.")
    (bitraverse getMaybeLength pure $ cleanStemLength t)
