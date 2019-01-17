{- |
Module      :  EvoMod.Data.Tree.Tree
Description :  Functions related to phylogenetic trees.
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 09:57:29 2019.

Comment about nomenclature:

- Trees have /nodes/ (not labels like in 'Data.Tree').

- Branches have /lengths/, not distances or times.

TODO:

- Export Tree (see BuilderLabel in Phylo.hs).

- Separate Measurable, Phylo, if possible.

- Use functional graph library for unrooted trees see also the book /Haskell
  high performance programming from Thomasson/, p. 344.

-}


module EvoMod.Data.Tree.Tree
  ( singleton
  , degree
  , rootNodesAgreeWith
  ) where

import Data.Tree
import qualified Data.Set as S

-- | The simplest tree. Usually an extant leaf with an attached branch.
singleton :: a -> Tree a
singleton n = Node n []

-- | The degree of the root node of a tree.
degree :: Tree a -> Int
degree = (+ 1) . length . subForest

-- | Check if ancestor and daughters of first tree are a subset of the ancestor
-- and daughters of the second tree. Useful to test if, e.g., speciations agree.
rootNodesAgreeWith :: (Ord c) => (a -> c) -> Tree a -> (b -> c) -> Tree b -> Bool
rootNodesAgreeWith f s g t =
  f (rootLabel s) == g (rootLabel t) &&
  S.fromList sDs `S.isSubsetOf` S.fromList tDs
  where sDs = map (f . rootLabel) (subForest s)
        tDs = map (g . rootLabel) (subForest t)
