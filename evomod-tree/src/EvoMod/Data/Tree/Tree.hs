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
  , leaves
  -- , rootNodesAgreeWith
  , subTree
  , pruneWith
  ) where

import           Data.Maybe
import           Data.Tree

-- | The simplest tree. Usually an extant leaf.
singleton :: a -> Tree a
singleton l = Node l []

-- | The degree of the root node of a tree.
degree :: Tree a -> Int
degree = (+ 1) . length . subForest

-- | Get leaves of tree.
leaves :: Tree a -> [a]
leaves (Node l []) = [l]
leaves (Node _ f)  = concatMap leaves f

-- -- | Check if ancestor and daughters of first tree are a subset of the ancestor
-- -- and daughters of the second tree. Useful to test if, e.g., speciations agree.
-- rootNodesAgreeWith :: (Ord c) => (a -> c) -> Tree a -> (b -> c) -> Tree b -> Bool
-- rootNodesAgreeWith f s g t =
--   f (rootLabel s) == g (rootLabel t) &&
--   S.fromList sDs `S.isSubsetOf` S.fromList tDs
--   where sDs = map (f . rootLabel) (subForest s)
--         tDs = map (g . rootLabel) (subForest t)

-- | Get subtree of 'Tree' with nodes satisfying predicate. Return 'Nothing', if
-- no leaf satisfies predicate. At the moment: recursively, for each child, take
-- the child if any leaf in the child satisfies the predicate.
subTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
subTree p leaf@(Node lbl [])
  | p lbl     = Just leaf
  | otherwise = Nothing
subTree p (Node lbl chs) = if null subTrees
                           then Nothing
                           else Just $ Node lbl subTrees
  where subTrees = mapMaybe (subTree p) chs

-- | Prune degree 2 inner nodes. The information stored in a pruned node can be
-- used to change the daughter node. To discard this information, use,
-- @pruneWith const tree@, otherwise @pruneWith (\daughter parent -> combined)
-- tree@.
pruneWith :: (a -> a -> a) -> Tree a -> Tree a
pruneWith _    n@(Node _ [])       = n
pruneWith join   (Node paLbl [ch]) = let lbl = join (rootLabel ch) paLbl
                                     in pruneWith join $ Node lbl (subForest ch)
pruneWith _    n                   = n
