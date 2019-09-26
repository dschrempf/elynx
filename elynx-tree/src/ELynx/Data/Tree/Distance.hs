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

-- -- Difference between two 'Set's, see 'Set.difference'. Do not compare elements
-- -- directly but apply a function beforehand.
-- differenceWith :: (Ord a, Ord b) => (a -> b) -> Set.Set a -> Set.Set a -> Set.Set a
-- differenceWith f xs ys = Set.filter (\e -> f e `Set.notMember` ys') xs
--   where ys' = Set.map f ys

-- -- Symmetric difference between two 'Set's. Do not compare elements directly but
-- -- apply a function beforehand.
-- symmetricDifferenceWith :: (Ord a, Ord b) => (a -> b) -> Set.Set a -> Set.Set a -> Set.Set a
-- symmetricDifferenceWith f xs ys = xsNotInYs `Set.union` ysNotInXs
--   where
--     xsNotInYs = differenceWith f xs ys
--     ysNotInXs = differenceWith f ys xs

-- Symmetric difference between two 'Set's.
symmetricDifference :: Ord a => S.Set a -> S.Set a -> S.Set a
symmetricDifference xs ys = S.difference xs ys `S.union` S.difference ys xs

-- -- Symmetric difference between two 'Map's.
-- symmetricDifferenceM :: Ord k => M.Map k a -> M.Map k a -> M.Map k a
-- symmetricDifferenceM x y = M.difference x y `M.union` M.difference y x

-- | Symmetric (Robinson-Foulds) distance between two trees. Before comparing
-- the leaf labels, apply a given function. This is useful, for example, to
-- compare the labels of 'Named' trees on their names only. The tree is assumed
-- to be UNROOTED!
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
symmetricWith :: (Ord b) => (a -> b) -> Tree a -> Tree a -> Int
symmetricWith f t1 t2 = length $ symmetricDifference (bs t1) (bs t2)
  where bs t = bipartitions $ fmap f t

-- | See 'symmetricWith', but with 'id' for comparisons.
symmetric :: Ord a => Tree a -> Tree a -> Int
symmetric = symmetricWith id

-- | Number of incompatible splits. Similar to 'symmetricWith' but
-- merges multifurcations.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
incompatibleSplitsWith :: (Ord b, Show b) => (a -> b) -> Tree a -> Tree a -> Int
incompatibleSplitsWith f t1 t2 = length $ symmetricDifference (ms t1) (ms t2)
  where ms t = bipartitionsCombined $ fmap f t

-- | See 'incompatibleSplitsWith', use 'id' for comparisons.
incompatibleSplits :: (Ord a, Show a) => Tree a -> Tree a -> Int
incompatibleSplits = incompatibleSplitsWith id

-- | Compute branch score distance between two trees. Before comparing the leaf
-- labels, apply a function. This is useful, for example, to compare the labels
-- of 'Named' trees on their names only. The branch information which is
-- compared to compute the distance is extracted from the nodes with a given
-- function. Assumes that the trees are UNROOTED.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
branchScoreWith :: (Ord a, Ord b, Floating c)
                        => (a -> b) -- ^ Label to compare on
                        -> (a -> c) -- ^ Branch information (e.g., length)
                                    -- associated with a node
                        -> Tree a -> Tree a -> c
branchScoreWith f g t1 t2 = sqrt dsSquared
  where bs        = bipartitionToBranch f (Sum . g)
        dBs       = M.map getSum $ M.unionWith (-) (bs t1) (bs t2)
        dsSquared = foldl' (\acc e -> acc + e*e) 0 dBs

-- | See 'branchScoreWith', use 'id' for comparisons.
branchScore :: (Ord a, Measurable a, Named a) => Tree a -> Tree a -> Double
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

