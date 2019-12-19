{- |
Module      :  ELynx.Data.Tree.Distance
Description :  Compute distances between trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jun 13 17:15:54 2019.

Various distance functions for phylogenetic trees (and binary trees in general).
All trees are assumed to be UNROOTED.

-}

module ELynx.Data.Tree.Distance
  ( symmetric
  , symmetricWith
  , compatible
  , incompatibleSplits
  , incompatibleSplitsWith
  , branchScore
  , branchScoreWith
  , pairwise
  , adjacent
  ) where

import           Data.List
import qualified Data.Map                       as M
import           Data.Monoid
import qualified Data.Set                       as S
import           Data.Tree

import           ELynx.Data.Tree.Bipartition
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.Multipartition
import           ELynx.Data.Tree.Subgroup

-- Symmetric difference between two 'Set's.
symmetricDifference :: Ord a => S.Set a -> S.Set a -> S.Set a
symmetricDifference xs ys = S.difference xs ys `S.union` S.difference ys xs

-- | Symmetric (Robinson-Foulds) distance between two trees. Before comparing
-- the leaf labels, apply a given function. This is useful, for example, to
-- compare the labels of 'ELynx.Data.Tree.NamedTree.Named' trees on their names
-- only. The tree is assumed to be UNROOTED!
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
symmetricWith :: (Ord b) => (a -> b) -> Tree a -> Tree a -> Int
symmetricWith f t1 t2 = length $ symmetricDifference (bs t1) (bs t2)
  where bs t = bipartitions $ fmap f t

-- | See 'symmetricWith', but with 'id' for comparisons.
symmetric :: (Ord a, Named a) => Tree a -> Tree a -> Int
symmetric = symmetricWith getName

countIncompatibilities :: (Ord a) => S.Set (Bipartition a) -> S.Set (Multipartition a) -> Int
countIncompatibilities bs ms = foldl' (\i b -> if any (compatible (fst $ bps b)) ms
                                               then i
                                               else i+1) 0 bs

-- | Number of incompatible splits. Similar to 'symmetricWith' but all
-- bipartition induced by multifurcations are considered.
--
-- A multifurcation on a tree may (but not necessarily does) represent missing
-- information about the order of bifurcations. In this case, it is interesting
-- to get a set of compatible bifurcations of the tree. For example, the tree
--
-- > (A,(B,C,D))
--
-- induces the following bipartitions:
--
-- > A|BCD
-- > B|ACD
--
-- > C|ABD
-- > D|ABC
--
-- Those are also reported by 'bipartitions'. However, it is additionally compatible with
--
-- > AB|CD
-- > AC|BD
-- > AD|BC
--
--
-- To check if a bipartition is compatible with a multipartition, the following
-- algorithm is used.
--
-- 1. Take one partition of the bipartition (at the moment the first, in the
-- future maybe the shorter one).
--
-- 2. For each leaf of the taken partition, add the partition of the
-- multipartition containing the leaf (takeLeaves and takeLeaf).
--
-- 3. If the resulting set of leafs (the union of the added partitions) does not
-- contain leaves of the other partition of the bipartitions, I am OK!
--
-- Only if a bipartition is not compatible with all induced multifurcations of
-- the other tree, it is incompatible.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
incompatibleSplitsWith :: (Ord b) => (a -> b) -> Tree a -> Tree a -> Int
incompatibleSplitsWith f t1 t2 = countIncompatibilities putIncBs1 ms2 +
                                 countIncompatibilities putIncBs2 ms1
  where
    -- Bipartitions.
    bs t = bipartitions $ fmap f t
    bs1 = bs t1
    bs2 = bs t2
    -- Putative incompatible bipartitions of trees one and two, respectively.
    putIncBs1 = bs1 S.\\ bs2
    putIncBs2 = bs2 S.\\ bs1
    -- Multipartitions.
    ms t = multipartitions $ fmap f t
    ms1 = ms t1
    ms2 = ms t2

-- | See 'incompatibleSplitsWith', use 'id' for comparisons.
incompatibleSplits :: (Ord a, Named a) => Tree a -> Tree a -> Int
incompatibleSplits = incompatibleSplitsWith getName

-- | Compute branch score distance between two trees. Before comparing the leaf
-- labels, apply a function. This is useful, for example, to compare the labels
-- of 'ELynx.Data.Tree.NamedTree.Named' trees on their names only. The branch
-- information which is compared to compute the distance is extracted from the
-- nodes with a given function. Assumes that the trees are UNROOTED.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
branchScoreWith :: (Ord a, Ord b, Floating c)
                        => (a -> b) -- ^ Label to compare on
                        -> (a -> c) -- ^ Branch information (e.g., length)
                                    -- associated with a node
                        -> Tree a -> Tree a -> c
branchScoreWith f g t1 t2 = sqrt dsSquared
  where bs        = bipartitionToBranchLength f (Sum . g)
        dBs       = M.map getSum $ M.unionWith (-) (bs t1) (bs t2)
        dsSquared = foldl' (\acc e -> acc + e*e) 0 dBs

-- | See 'branchScoreWith', use 'id' for comparisons.
branchScore :: (Ord a, Named a, Measurable a) => Tree a -> Tree a -> Double
branchScore = branchScoreWith getName getLen

-- | Compute pairwise distances of a list of input trees. Use given distance
-- measure. Returns a triple, the first two elements are the indices of the
-- compared trees, the third is the distance.
pairwise :: (a -> a -> b)   -- ^ Distance function
         -> [a]             -- ^ Input trees
         -> [(Int, Int, b)] -- ^ (index i, index j, distance i j)
pairwise dist trs = [ (i, j, dist x y)
                    | (i:is, x:xs) <- zip (tails [0..]) (tails trs)
                    , (j, y) <- zip is xs ]

-- | Compute distances between adjacent pairs of a list of input trees. Use
-- given distance measure.
adjacent :: (Tree a -> Tree a -> b) -- ^ Distance function
         -> [Tree a]                -- ^ Input trees
         -> [b]
adjacent dist trs = [ dist x y | (x, y) <- zip trs (tail trs) ]
