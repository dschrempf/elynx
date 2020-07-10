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
    roots,
    rootAt,
    connect,
    removeMultifurcations,
    extend,
    prune,
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

-- | A label type for phylogenetic trees with branch length and branch support.
data PhyloLabel a = PhyloLabel
  { label :: a,
    brLen :: BranchLength,
    brSup :: BranchSupport
  }
  deriving (Read, Show, Eq, Ord)

instance Measurable (PhyloLabel a) where
  getLen = brLen
  setLen x l = l {brLen = x}

instance BranchSupported (PhyloLabel a) where
  getBranchSupport = brSup
  setBranchSupport Nothing l = l {brSup = Nothing}
  setBranchSupport (Just s) l
    | s > 0 = l {brSup = Just s}
    | otherwise = error "Branch support cannot be zero nor negative."

instance Named a => Named (PhyloLabel a) where
  getName = getName . label

halve :: Measurable a => a -> a
halve l = setLen (getLen l / 2.0) l

type PhyloTree a = Tree (PhyloLabel a)

-- | For a rooted, bifurcating tree, get all possible rooted trees.
--
-- For a tree with @n>2@ leaves, there are @(2n-3)@ rooted trees. The root node
-- is moved. Branch lengths and branch support are merged according to the
-- 'Measurable' and 'BranchSupported' instances.
--
-- An error is thrown if the tree is not 'bifurcating'.
roots :: PhyloTree a -> [PhyloTree a]
roots (Node c []) = [Node c []]
roots t@(Node _ [Node _ [], Node _ []]) = [t]
roots t@(Node _ [_, _]) = t : lefts t ++ rights t
roots _ = error "roots: Tree is not bifurcating."

-- Move the root to the left.
lefts :: PhyloTree a -> [PhyloTree a]
lefts (Node c [Node l [Node x xs, Node y ys], Node r rs])
  | getBranchSupport l /= getBranchSupport r = error "lefts: Unequal branch support."
  | otherwise =
    let -- Left.
        x' = halve x
        l' = setBranchSupport (getBranchSupport x') $ setLen (getLen x') l
        r' = setLen (getLen r + getLen l) r
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
rights :: PhyloTree a -> [PhyloTree a]
rights (Node c [Node l ls, Node r [Node x xs, Node y ys]])
  | getBranchSupport l /= getBranchSupport r = error "rights: Unequal branch support."
  | otherwise =
    let -- Left.
        x' = halve x
        r' = setBranchSupport (getBranchSupport x') $ setLen (getLen x') r
        l' = setLen (getLen r + getLen l) l
        tl = Node c [Node x' xs, Node r' [Node y ys, Node l' ls]]
        -- Right.
        y' = halve y
        r'' = setBranchSupport (getBranchSupport y') $ setLen (getLen y') r
        tr = Node c [Node r'' [Node l' ls, Node x xs], Node y' ys]
     in tl : tr : lefts tl ++ rights tr
rights (Node _ [_, Node _ []]) = []
rights (Node _ []) = error "rights: THIS ERROR SHOULD NEVER HAPPEN. Encountered a leaf."
rights _ = error "rights: Tree is not bifurcating."

-- | Root a bifurcating tree at a given point.
--
-- Root the tree at the midpoint of the branch defined by the given bipartition.
-- The original root node is moved to the new position.
--
-- - The tree has to be bifurcating (may be relaxed in the future).
-- - The leaves of the tree have to be unique.
-- - The leaves in the bipartition have to match the leaves of the tree.
rootAt :: Ord a => Bipartition (PhyloLabel a) -> PhyloTree a -> PhyloTree a
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
rootAt' :: (Eq a, Ord a) => Bipartition (PhyloLabel a) -> PhyloTree a -> Maybe (PhyloTree a)
rootAt' b t = find (\x -> b == bipartition x) (roots t)

-- | Connect two trees with a branch in all possible ways.
--
-- Basically, introduce a branch between two trees. If the trees have n, and m
-- branches, respectively, there are n*m ways to connect them.
--
-- A base node has to be given which will be used wherever the new node is
-- introduced.
connect :: PhyloLabel a -> PhyloTree a -> PhyloTree a -> [PhyloTree a]
connect n l r = [Node n [x, y] | x <- roots l, y <- roots r]

-- | Remove multifurcations by copying multifurcating nodes and introducing
-- branches without length and branch support.
removeMultifurcations :: PhyloTree a -> PhyloTree a
removeMultifurcations t@(Node _ []) = t
removeMultifurcations (Node l [x]) = Node l [removeMultifurcations x]
removeMultifurcations (Node l [x, y]) = Node l $ map removeMultifurcations [x, y]
removeMultifurcations (Node l (x : xs)) = Node l $ map removeMultifurcations [x, Node l' xs]
  where
    l' = setBranchSupport Nothing $ setLen (branchLength $ Just 1.0) l

-- | Add branch lengths, but forget all other information of parent node label.
-- Also reset branch support.
extend :: PhyloLabel a -> PhyloLabel a -> PhyloLabel a
extend da pa = setBranchSupport Nothing $ setLen (brLen pa + brLen da) da

-- | Prune degree 2 nodes. Use 'extend' and 'pruneWith'.
prune :: PhyloTree a -> PhyloTree a
prune = pruneWith extend
