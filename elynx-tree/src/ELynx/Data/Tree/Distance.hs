-- |
-- Module      :  ELynx.Data.Tree.Distance
-- Description :  Compute distances between trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jun 13 17:15:54 2019.
--
-- Various distance functions for phylogenetic trees (and trees with branch
-- lengths in general).
--
-- The functions provided in this module return distances for __unrooted__
-- trees. See comments of 'symmetric', 'branchScore', and 'bipartitionToBranch',
-- as well as the documentation of
-- [treedist](http://evolution.genetics.washington.edu/phylip/doc/treedist.html).
--
-- It is a little unfortunate that 'Tree' data type, which represents rooted
-- trees, is also used in this module. However, rooted trees are much easier to
-- handle. In the future, a separate data type for unrooted trees may be
-- introduced. In theory, this is quite straight forward, for example, using
-- algebraic graphs. Difficulties may arise because the branches of an unrooted
-- tree are undirected.
module ELynx.Data.Tree.Distance
  ( symmetric,
    incompatibleSplits,
    branchScore,
    -- adjacent,
  )
where

import Data.Bifunctor
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Data.Set (Set)
import ELynx.Data.Tree.Bipartition
import ELynx.Data.Tree.Partition
import ELynx.Data.Tree.Rooted

-- Symmetric difference between two 'Set's.
symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference xs ys = S.difference xs ys `S.union` S.difference ys xs

-- | Symmetric (Robinson-Foulds) distance between two trees.
--
-- Although a rooted tree data type is used, the distance between the unrooted
-- trees is returned.
--
-- Return 'Nothing' if the trees contain different leaves.
--
-- XXX: Comparing a list of trees may recompute bipartitions.
symmetric :: Ord a => Tree e a -> Tree e a -> Either String Int
symmetric t1 t2
  | S.fromList (leaves t1) /= S.fromList (leaves t2) = Left "symmetric: Trees contain different leaves."
  | otherwise = do
    bps1 <- bipartitions t1
    bps2 <- bipartitions t2
    return $ length $ symmetricDifference bps1 bps2

countIncompatibilities :: (Show a, Ord a) => Set (Bipartition a) -> Set (Partition a) -> Int
countIncompatibilities bs ms =
  foldl' (\i b -> if any (compatible $ bpToMp b) ms then i else i + 1) 0 bs

-- | Number of incompatible splits.
--
-- Similar to 'symmetric' but all bipartitions induced by multifurcations are
-- considered. For a detailed description of how the distance is calculated, see
-- 'ELynx.Data.Tree.Bipartition.bipartitionCompatible'.
--
-- A multifurcation on a tree may (but not necessarily does) represent missing
-- information about the order of bifurcations. In this case, it is interesting
-- to get a set of compatible bifurcations of the tree. For example, the star tree
--
-- > (A,B,C,D);
--
-- induces the following bipartitions:
--
-- > A|BCD
-- > B|ACD
-- > C|ABD
-- > D|ABC
--
-- However, the tree is additionally compatible with the following hidden
-- bipartitions:
--
-- > AB|CD
-- > AC|BD
-- > AD|BC
--
-- For an explanation of how compatibility of multipartitions is checked, see
-- 'compatible'. Before using 'compatible', bipartitions are simply converted to
-- multipartitions with two subsets.
--
-- A bipartition is incompatible with a tree if it is incompatible with all
-- induced multifurcations of the tree.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
incompatibleSplits :: (Show a, Ord a) => Tree e a -> Tree e a -> Either String Int
incompatibleSplits t1 t2
  | S.fromList (leaves t1) /= S.fromList (leaves t2) =
    Left "incompatibleSplits: Trees do not have equal leaf sets."
  | otherwise = do
    -- Bipartitions.
    bs1 <- bipartitions t1
    bs2 <- bipartitions t2
    -- traceShowM $ "bs1" ++ show (S.map bpHuman bs1)
    -- traceShowM $ "bs2" ++ show (S.map bpHuman bs2)
    let -- Putative incompatible bipartitions of trees one and two, respectively.
        putIncBs1 = bs1 S.\\ bs2
        putIncBs2 = bs2 S.\\ bs1
    -- Partitions.
    ms1 <- multipartitions t1
    ms2 <- multipartitions t2
    -- traceShowM $ "putIncBs1 " ++ show (S.map bpHuman putIncBs1)
    -- traceShowM $ "putIncBs2 " ++ show (S.map bpHuman putIncBs2)
    return $ countIncompatibilities putIncBs1 ms2 + countIncompatibilities putIncBs2 ms1

-- | Compute branch score distance between two trees.
--
-- Although a rooted tree data type is used, the distance between the unrooted
-- trees is returned.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
branchScore :: (Floating e, Ord a) => Tree e a -> Tree e a -> Either String e
branchScore t1 t2
  | S.fromList (leaves t1) /= S.fromList (leaves t2) = Left "branchScoreWith: Trees do not have equal leaf sets."
  | otherwise = do
    bpToBr1 <- bipartitionToBranch $ first Sum t1
    bpToBr2 <- bipartitionToBranch $ first Sum t2
    let dBs = M.unionWith (-) bpToBr1 bpToBr2
        dsSquared = foldl' (\acc e -> let e' = getSum e in acc + e' * e') 0 dBs
    return $ sqrt dsSquared
