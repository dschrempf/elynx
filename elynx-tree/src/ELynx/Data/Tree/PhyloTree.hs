{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      :  ELynx.Data.Tree.PhyloTree
-- Description :  Phylogenetic trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 16:08:54 2019.
--
-- Phylogenetic nodes labels, aka 'PhyloLabel's, have a branch length and an
-- arbitrary label type, e.g., of type 'Int'.
module ELynx.Data.Tree.PhyloTree
  ( PhyloLabel (..),
    removeBrInfo,
    rootAt,
    roots,
    connect,
  )
where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Tree
import ELynx.Data.Tree.Bipartition
import ELynx.Data.Tree.BranchSupportTree
import ELynx.Data.Tree.MeasurableTree
import ELynx.Data.Tree.Tree
import ELynx.Data.Tree.NamedTree

-- | A primitive label type for phylogenetic trees with a name, possibly a
-- branch support value, and possibly a branch length.
data PhyloLabel a = PhyloLabel
  { label :: a,
    -- TODO: This is awkward. Either all branches have support, or none.
    brSup :: Maybe Double,
    -- TODO: This is awkward. Either all branches have a length, or none.
    brLen :: Maybe Double
  }
  deriving (Read, Show, Eq, Ord)

-- XXX: 2020-07-10. I changed this instance; this may have been a breaking change.
-- instance Ord a => Ord (PhyloLabel a) where
--   compare = compare `on` label

-- | If no branch length is available, 0 is returned. This is probably not the
-- best (and final) behavior.
instance Measurable (PhyloLabel a) where
  -- TODO: See 'roots'. The fromMaybe should not be used.
  getLen = fromMaybe 0 . brLen
  setLen l x
    | l >= 0 = x {brLen = Just l}
    | otherwise = error $ "Branch lengths cannot be negative: " <> show l

instance BranchSupported (PhyloLabel a) where
  getBranchSupport = brSup
  setBranchSupport Nothing l = l {brSup = Nothing}
  setBranchSupport (Just s) l
    | s > 0 = l {brSup = Just s}
    | otherwise = error "Branch support cannot be zero nor negative."

instance Named a => Named (PhyloLabel a) where
  getName = getName . label

-- | Remove all branch relevant information from all nodes of the tree; only
-- retain the labels.
removeBrInfo :: Tree (PhyloLabel a) -> Tree a
removeBrInfo = fmap label

-- | Root a bifurcating tree at a given point.
--
-- Root the tree at the midpoint of the branch defined by the given bipartition.
-- The original root node is moved to the new position.
--
-- - The tree has to be bifurcating (may be relaxed in the future).
-- - The leaves of the tree have to be unique.
-- - The leaves in the bipartition have to match the leaves of the tree.
rootAt :: (BranchSupported a, Measurable a, Ord a) => Bipartition a -> Tree a -> Tree a
rootAt b t
  -- Tree is checked for being bifurcating in 'roots'.
  | length ls /= S.size lS = error "rootAt: Leaves of tree are not unique."
  | bS /= lS = error "rootAt: Bipartition does not match leaves of tree."
  | otherwise =
    fromMaybe
      (error "rootAt: Bipartition not found on tree.")
      (rootAt' b t)
  where
    bS = toSet b
    ls = S.fromList $ leaves t
    lS = S.fromList $ leaves t

-- Assume the leaves of the tree are unique.
rootAt' :: (BranchSupported a, Eq a, Measurable a, Ord a) => Bipartition a -> Tree a -> Maybe (Tree a)
-- XXX: This would lead to errors if the root of the tree is not bifurcating.
rootAt' b t = find (\x -> b == bipartition x) (roots t)

-- -- | For a rooted, bifurcating tree, get all possible rooted trees.
-- --
-- -- For a tree with @n>2@ leaves, there are @(2n-3)@ rooted trees. The root node
-- -- is moved. Branch lengths and branch support are merged according to the
-- -- 'Measurable' and 'BranchSupported' instances.
-- --
-- -- An error is thrown if the tree is not 'bifurcating'.
-- roots :: Tree a -> [Tree a]
-- -- Leaves, and cherries have to be handled separately, because they cannot be
-- -- rotated.
-- roots t@(Node _ []) = [t]
-- roots t@(Node _ [Node _ [], Node _ []]) = [t]
-- roots t
--   -- XXX: Do we need the check here, or can we have it while rotating?
--   | bifurcating t = t : left t ++ right t
--   | otherwise = error "roots: Tree is not bifurcating."

halve :: Measurable a => a -> a
halve l = setLen (getLen l / 2.0) l

-- TODO: This will fail if 'PhyloLabel' is used and some branch lengths are actually 'Nothing'.
-- | For a rooted, bifurcating tree, get all possible rooted trees.
--
-- For a tree with @n>2@ leaves, there are @(2n-3)@ rooted trees. The root node
-- is moved. Branch lengths and branch support are merged according to the
-- 'Measurable' and 'BranchSupported' instances.
--
-- An error is thrown if the tree is not 'bifurcating'.
roots :: (Measurable a, BranchSupported a) => Tree a -> [Tree a]
roots (Node c []) = [Node c []]
roots t@(Node _ [Node _ [], Node _ []]) = [t]
roots t@(Node _ [_, _]) = t : lefts t ++ rights t
roots _ = error "roots: Tree is not bifurcating."

-- Move the root to the left.
lefts :: (Measurable a, BranchSupported a) => Tree a -> [Tree a]
lefts (Node c [Node l [Node x xs, Node y ys], Node r rs])
  | getBranchSupport l /= getBranchSupport r = error "lefts: Unequal branch support."
  | otherwise =
    let -- Left.
        x' = halve x
        l' = setBranchSupport (getBranchSupport x') $ setLen (getLen x') l
        r' = lengthen (getLen l) r
        tl = Node c [Node x' xs, Node l' [Node y ys, Node r' rs]]
        -- Right.
        y' = halve y
        l'' = setBranchSupport (getBranchSupport y') $ setLen (getLen y') l
        tr = Node c [Node l'' [Node r' rs, Node x xs], Node y' ys]
     in tl : tr : lefts tl ++ rights tr
lefts (Node _ [Node _ [], _]) = []
lefts (Node _ []) = error "lefts: THIS ERROR SHOULD NEVER HAPPEN. Encountered a leaf."
lefts _ = error "lefts: Tree is not bifurcating."

-- Move the root to the right.
rights :: (Measurable a, BranchSupported a) => Tree a -> [Tree a]
rights (Node c [Node l ls, Node r [Node x xs, Node y ys]])
  | getBranchSupport l /= getBranchSupport r = error "rights: Unequal branch support."
  | otherwise =
    let -- Left.
        x' = halve x
        r' = setBranchSupport (getBranchSupport x') $ setLen (getLen x') r
        l' = lengthen (getLen r) l
        tl = Node c [Node x' xs, Node r' [Node y ys, Node l' ls]]
        -- Right.
        y' = halve y
        r'' = setBranchSupport (getBranchSupport y') $ setLen (getLen y') r
        tr = Node c [Node r'' [Node l' ls, Node x xs], Node y' ys]
     in tl : tr : lefts tl ++ rights tr
rights (Node _ [_, Node _ []]) = []
rights (Node _ []) = error "rights: THIS ERROR SHOULD NEVER HAPPEN. Encountered a leaf."
rights _ = error "rights: Tree is not bifurcating."

-- | Connect two trees with a branch in all possible ways.
--
-- Basically, introduce a branch between two trees. If the trees have n, and m
-- branches, respectively, there are n*m ways to connect them.
--
-- A base node has to be given which will be used wherever the new node is
-- introduced.
connect :: (BranchSupported a, Measurable a) => a -> Tree a -> Tree a -> [Tree a]
connect n l r = [Node n [x, y] | x <- roots l, y <- roots r]
