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
import           ELynx.Data.Tree.Multipartition
import           ELynx.Data.Tree.Partition

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
-- compare the labels of 'ELynx.Data.Tree.NamedTree.Named' trees on their names
-- only. The tree is assumed to be UNROOTED!
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
symmetricWith :: (Ord b) => (a -> b) -> Tree a -> Tree a -> Int
symmetricWith f t1 t2 = length $ symmetricDifference (bs t1) (bs t2)
  where bs t = bipartitions $ fmap f t

-- | See 'symmetricWith', but with 'id' for comparisons.
symmetric :: Ord a => Tree a -> Tree a -> Int
symmetric = symmetricWith id

-- TODO Too slow.
-- -- | Number of incompatible splits. Similar to 'symmetricWith' but
-- -- merges multifurcations.
-- --
-- -- XXX: Comparing a list of trees with this function recomputes bipartitions.
-- incompatibleSplitsWith :: (Ord b, Show b) => (a -> b) -> Tree a -> Tree a -> Int
-- incompatibleSplitsWith f t1 t2 = S.size (bs t1 S.\\ cs t2) + S.size (bs t2 S.\\ cs t1)
--   -- where ms t = bipartitionsCombined $ fmap f t
--   where bs t = bipartitions $ fmap f t
--         cs t = bipartitionsCompatible $ fmap f t

takeLeaf :: Ord a => Multipartition a -> Partition a -> a -> Partition a
takeLeaf m ls l | l `pmember` ls = ls
                | otherwise      = ls `punion` findMp l m

takeLeaves :: Ord a => Partition a -> Multipartition a -> Partition a
takeLeaves ls m = foldl' (takeLeaf m) pempty ls

-- Is a bipartition compatible with a multipartition?
--
-- The idea is the following.
--
-- 1. Take one partition of the bipartition (the first, or the shorter one).
--
-- 2. For each leaf of the partition, add the partition of the multipartition
-- containing the leaf.
--
-- 3. If the resulting set of leafs (the union of the added partitions) does not
-- contain leaves of the other partition of the bipartitions (the second, or the
-- longer one), I am OK!
compatible :: Ord a => Bipartition a -> Multipartition a -> Bool
compatible b m = (ls `pdifference` takeLeaves ls m) == pempty
  where ls = fst $ bps b

countIncompatibilities :: Ord a => S.Set (Bipartition a) -> S.Set (Multipartition a) -> Int
countIncompatibilities bs ms = foldl' (\i b -> if any (compatible b) ms then i else i+1) 0 bs

-- | Number of incompatible splits. Similar to 'symmetricWith' but
-- merges multifurcations.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
incompatibleSplitsWith :: (Ord b, Show b) => (a -> b) -> Tree a -> Tree a -> Int
incompatibleSplitsWith f t1 t2 = countIncompatibilities putIncBs1 ms2 +
                                 countIncompatibilities putIncBs2 ms1
  -- TODO: Now we have to check if the putative differences are actually compatible.
  where bs t = bipartitions $ fmap f t
        bs1 = bs t1
        bs2 = bs t2
        ms t = multipartitions $ fmap f t
        ms1 = ms t1
        ms2 = ms t2
        -- Putative incompatible bipartitions of trees one and two, respectively.
        putIncBs1 = bs1 S.\\ bs2
        putIncBs2 = bs2 S.\\ bs1

-- | See 'incompatibleSplitsWith', use 'id' for comparisons.
incompatibleSplits :: (Ord a, Show a) => Tree a -> Tree a -> Int
incompatibleSplits = incompatibleSplitsWith id

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
  where bs        = bipartitionToBranch f (Sum . g)
        dBs       = M.map getSum $ M.unionWith (-) (bs t1) (bs t2)
        dsSquared = foldl' (\acc e -> acc + e*e) 0 dBs

-- | See 'branchScoreWith', use 'id' for comparisons.
branchScore :: (Ord a, Measurable a) => Tree a -> Tree a -> Double
branchScore = branchScoreWith id getLen

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

