-- |
-- Module      :  ELynx.Data.Tree.Topology
-- Description :  Topologies
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jul 11 10:28:28 2020.
--
-- A 'Topology' is a tree without branch labels and without internal node
-- labels. The leaves have unique labels.
--
-- The order of children is arbitrary. Internally, 'Set's are used instead of
-- lists like for rose trees (see 'Data.Tree').
module ELynx.Data.Tree.Topology
  ( Topology (..),
    fromTree,
    leaves,
  )
where

import Data.Function
import Data.Tree
import qualified Data.Set as S
import Data.Set (Set)

data Topology a = TN { children ::Set (Topology a) }
                | TL { label :: a }
                deriving (Eq)

instance Ord a => Ord (Topology a) where
  compare = compare `on` leaves

-- | Convert a tree to a topology. Internal node labels are lost.
fromTree :: Ord a => Tree a -> Topology a
fromTree (Node x xs) = TN (S.fromList $ map fromTree xs)

-- | Set of leaves.
leaves :: Ord a => Topology a -> Set a
leaves (TN xs) = S.unions $ S.map leaves xs
leaves (TL x)  = S.singleton x

-- -- TODO: Decide which of these functions should be provided for more general
-- -- data structures. Provide some of these functions only for @Topology@ (which
-- -- has to be defined) and @Tree (Length a)@.

-- halve :: PhyloLabel a -> PhyloLabel a
-- halve l = setLen (getLen l / 2.0) l

-- -- | For a rooted, bifurcating tree, get all possible rooted trees.
-- --
-- -- For a tree with @n>2@ leaves, there are @(2n-3)@ rooted trees. The root node
-- -- is moved. Branch lengths and branch support are merged according to the
-- -- 'Measurable' and 'BranchSupported' instances.
-- --
-- -- An error is thrown if the tree is not 'bifurcating'.
-- roots :: Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
-- roots (Node c []) = [Node c []]
-- roots t@(Node _ [Node _ [], Node _ []]) = [t]
-- roots t@(Node _ [_, _]) = t : lefts t ++ rights t
-- roots _ = error "roots: Tree is not bifurcating."

-- -- Move the root to the left.
-- lefts :: Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
-- lefts (Node c [Node l [Node x xs, Node y ys], Node r rs])
--   | getBranchSupport l /= getBranchSupport r = error "lefts: Unequal branch support."
--   | otherwise =
--     let -- Left.
--         x' = halve x
--         l' = setBranchSupport (getBranchSupport x') $ setLen (getLen x') l
--         r' = setLen (getLen r + getLen l) r
--         tl = Node c [Node x' xs, Node l' [Node y ys, Node r' rs]]
--         -- Right.
--         y' = halve y
--         l'' = setBranchSupport (getBranchSupport y') $ setLen (getLen y') l
--         tr = Node c [Node l'' [Node r' rs, Node x xs], Node y' ys]
--      in tl : tr : lefts tl ++ rights tr
-- lefts (Node _ [Node _ [], _]) = []
-- lefts (Node _ []) = error "lefts: THIS ERROR SHOULD NEVER HAPPEN. Encountered a leaf."
-- lefts _ = error "lefts: Tree is not bifurcating."

-- -- Move the root to the right.
-- rights :: Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
-- rights (Node c [Node l ls, Node r [Node x xs, Node y ys]])
--   | getBranchSupport l /= getBranchSupport r = error "rights: Unequal branch support."
--   | otherwise =
--     let -- Left.
--         x' = halve x
--         r' = setBranchSupport (getBranchSupport x') $ setLen (getLen x') r
--         l' = setLen (getLen r + getLen l) l
--         tl = Node c [Node x' xs, Node r' [Node y ys, Node l' ls]]
--         -- Right.
--         y' = halve y
--         r'' = setBranchSupport (getBranchSupport y') $ setLen (getLen y') r
--         tr = Node c [Node r'' [Node l' ls, Node x xs], Node y' ys]
--      in tl : tr : lefts tl ++ rights tr
-- rights (Node _ [_, Node _ []]) = []
-- rights (Node _ []) = error "rights: THIS ERROR SHOULD NEVER HAPPEN. Encountered a leaf."
-- rights _ = error "rights: Tree is not bifurcating."

-- -- | Root a bifurcating tree at a given point.
-- --
-- -- Root the tree at the midpoint of the branch defined by the given bipartition.
-- -- The original root node is moved to the new position.
-- --
-- -- - The tree has to be bifurcating (may be relaxed in the future).
-- -- - The leaves of the tree have to be unique.
-- -- - The leaves in the bipartition have to match the leaves of the tree.
-- rootAt :: Ord a => Bipartition (PhyloLabel a) -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- rootAt b t
--   -- Tree is checked for being bifurcating in 'roots'.
--   | length ls /= S.size lS = error "rootAt: Leaves of tree are not unique."
--   | bS /= lS = error "rootAt: Bipartition does not match leaves of tree."
--   | otherwise =
--     fromMaybe
--       (error "rootAt: Bipartition not found on tree.")
--       (rootAt' b t)
--   where
--     bS = toSet b
--     ls = S.fromList $ leaves t
--     lS = S.fromList $ leaves t

-- -- Assume the leaves of the tree are unique.
-- rootAt' :: (Eq a, Ord a) => Bipartition (PhyloLabel a) -> Tree (PhyloLabel a) -> Maybe (Tree (PhyloLabel a))
-- rootAt' b t = find (\x -> b == bipartition x) (roots t)

-- -- | Connect two trees with a branch in all possible ways.
-- --
-- -- Basically, introduce a branch between two trees. If the trees have n, and m
-- -- branches, respectively, there are n*m ways to connect them.
-- --
-- -- A base node has to be given which will be used wherever the new node is
-- -- introduced.
-- connect :: PhyloLabel a -> Tree (PhyloLabel a) -> Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
-- connect n l r = [Node n [x, y] | x <- roots l, y <- roots r]

-- -- | Remove multifurcations by copying multifurcating nodes and introducing
-- -- branches with length 0 and branch support 0.
-- removeMultifurcations :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- removeMultifurcations t@(Node _ []) = t
-- removeMultifurcations (Node l [x]) = Node l [removeMultifurcations x]
-- removeMultifurcations (Node l [x, y]) = Node l $ map removeMultifurcations [x, y]
-- removeMultifurcations (Node l (x : xs)) = Node l $ map removeMultifurcations [x, Node l' xs]
--   where
--     l' = l {brLen = 0, brSup = 0}

-- -- | Add branch lengths. Set branch support to the lower support value. Forget
-- -- the parent node label.
-- extend :: PhyloLabel a -> PhyloLabel a -> PhyloLabel a
-- extend da pa = da {brSup = min (brSup pa) (brSup da), brLen = brLen pa + brLen da}

-- -- | Prune degree 2 nodes. Use 'extend' and 'pruneWith'.
-- prune :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- prune = pruneWith extend
