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

- In 'Data.Tree', a 'Tree' is defined as

@
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }
@

This means, that the word 'Node' is reserved for the constructor of a tree, and
has a label and a children. The terms 'Node' and /label/ are not to be confused.

- Branches have /lengths/. For example, a branch length can be a distances or a
  time.

XXX: Try fgl or alga. Use functional graph library for unrooted trees see also
the book /Haskell high performance programming from Thomasson/, p. 344.

-}


module EvoMod.Data.Tree.Tree
  ( singleton
  , degree
  , leafs
  -- , rootNodesAgreeWith
  ) where

import Data.Tree
-- import qualified Data.Set as S

-- | The simplest tree. Usually an extant leaf.
singleton :: a -> Tree a
singleton l = Node l []

-- | The degree of the root node of a tree.
degree :: Tree a -> Int
degree = (+ 1) . length . subForest

leafs :: Tree a -> [a]
leafs (Node l []) = [l]
leafs (Node _ f)  = concatMap leafs f

-- -- | Check if ancestor and daughters of first tree are a subset of the ancestor
-- -- and daughters of the second tree. Useful to test if, e.g., speciations agree.
-- rootNodesAgreeWith :: (Ord c) => (a -> c) -> Tree a -> (b -> c) -> Tree b -> Bool
-- rootNodesAgreeWith f s g t =
--   f (rootLabel s) == g (rootLabel t) &&
--   S.fromList sDs `S.isSubsetOf` S.fromList tDs
--   where sDs = map (f . rootLabel) (subForest s)
--         tDs = map (g . rootLabel) (subForest t)
