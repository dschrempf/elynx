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

TODO: All trees are assumed to be UNROOTED. See comments of 'symmetricWith' and
'branchScoreWith', as well as 'bipartitionToBranchLength'.
http://evolution.genetics.washington.edu/phylip/doc/treedist.html. However, this
disagrees with the statement in 'ELynx.Data.Tree.Tree', and should be changed.
I definitely need separate data types for rooted and unrooted trees.

TODO: Use subset module.

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
  )
where

import           Data.List
import qualified Data.Map                      as M
import           Data.Monoid
import qualified Data.Set                      as S
import           Data.Tree

import           ELynx.Data.Tree.Bipartition    ( Bipartition
                                                , bipartitionToBranchLength
                                                , bipartitions
                                                )
import           ELynx.Data.Tree.MeasurableTree ( Measurable
                                                , getLen
                                                )
import           ELynx.Data.Tree.Multipartition ( Multipartition
                                                , compatible
                                                , fromBipartition
                                                , multipartitions
                                                )
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.Tree           ( leaves )

-- Symmetric difference between two 'Set's.
symmetricDifference :: Ord a => S.Set a -> S.Set a -> S.Set a
symmetricDifference xs ys = S.difference xs ys `S.union` S.difference ys xs

-- | Symmetric (Robinson-Foulds) distance between two trees. Before comparing
-- the leaf labels, apply a given function. This is useful, for example, to
-- compare the labels of 'ELynx.Data.Tree.NamedTree.Named' trees on their names
-- only. The tree is assumed to be UNROOTED! See
-- http://evolution.genetics.washington.edu/phylip/doc/treedist.html.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
symmetricWith :: (Show a, Ord b) => (a -> b) -> Tree a -> Tree a -> Int
symmetricWith f t1 t2
  | S.fromList (leaves t1') /= S.fromList (leaves t2') = error
    "symmetricWith: trees do not have equal leaf sets."
  | otherwise = length
  $ symmetricDifference (bipartitions t1') (bipartitions t2')
 where
  t1' = fmap f t1
  t2' = fmap f t2

-- | See 'symmetricWith', but with 'getName' for comparisons.
symmetric :: (Show a, Ord a, Named a) => Tree a -> Tree a -> Int
symmetric = symmetricWith getName

countIncompatibilities
  :: (Ord a, Show a) => S.Set (Bipartition a) -> S.Set (Multipartition a) -> Int
countIncompatibilities bs ms = foldl'
  (\i b -> if any (compatible (fromBipartition b)) ms then i else i + 1)
  0
  bs

-- | Number of incompatible splits. Similar to 'symmetricWith' but all
-- bipartition induced by multifurcations are considered. For a detailed
-- description of how the distance is calculated, see 'compatible'.
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
-- Those are also reported by the function 'bipartitions'. However, the tree is
-- additionally compatible with the following hidden bipartitions:
--
-- > AB|CD
-- > AC|BD
-- > AD|BC
--
-- For an explanation of how compatibility of a bipartition with a
-- multipartition is checked, see 'compatible'. Before using 'compatible',
-- bipartitions are simply converted to multipartitions with two subsets.
--
-- Only if a bipartition is not compatible with all induced multifurcations of
-- the other tree, it is incompatible.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
incompatibleSplitsWith :: (Ord b, Show b) => (a -> b) -> Tree a -> Tree a -> Int
incompatibleSplitsWith f t1 t2
  | S.fromList (leaves t1') /= S.fromList (leaves t2')
  = error "incompatibleSplitsWith: trees do not have equal leaf sets."
  | otherwise
  = countIncompatibilities putIncBs1 ms2 + countIncompatibilities putIncBs2 ms1
 where
  t1'       = fmap f t1
  t2'       = fmap f t2
  bs1       = bipartitions t1'
  bs2       = bipartitions t2'
  -- Putative incompatible bipartitions of trees one and two, respectively.
  putIncBs1 = bs1 S.\\ bs2
  putIncBs2 = bs2 S.\\ bs1
  -- Multipartitions.
  ms t = multipartitions $ fmap f t
  ms1 = ms t1
  ms2 = ms t2

-- | See 'incompatibleSplitsWith', use 'getName' for comparisons.
incompatibleSplits :: (Ord a, Named a) => Tree a -> Tree a -> Int
incompatibleSplits = incompatibleSplitsWith getName

-- | Compute branch score distance between two trees. Before comparing the leaf
-- labels, apply a function. This is useful, for example, to compare the labels
-- of 'ELynx.Data.Tree.NamedTree.Named' trees on their names only. The branch
-- information which is compared to compute the distance is extracted from the
-- nodes with a given function. Assumes that the trees are UNROOTED. See
-- http://evolution.genetics.washington.edu/phylip/doc/treedist.html.
--
-- XXX: Comparing a list of trees with this function recomputes bipartitions.
branchScoreWith
  :: (Ord a, Ord b, Floating c)
  => (a -> b) -- ^ Label to compare on
  -> (a -> c) -- ^ Branch information (e.g., length)
                                    -- associated with a node
  -> Tree a
  -> Tree a
  -> c
branchScoreWith f g t1 t2
  | S.fromList (leaves . fmap f $ t1) /= S.fromList (leaves . fmap f $ t2)
  = error "branchScoreWith: trees do not have equal leaf sets."
  | otherwise
  = sqrt dsSquared
 where
  bs        = bipartitionToBranchLength f (Sum . g)
  dBs       = M.map getSum $ M.unionWith (-) (bs t1) (bs t2)
  dsSquared = foldl' (\acc e -> acc + e * e) 0 dBs

-- | See 'branchScoreWith', use 'getName' and 'getLen' for comparisons.
branchScore :: (Ord a, Named a, Measurable a) => Tree a -> Tree a -> Double
branchScore = branchScoreWith getName getLen

-- | Compute pairwise distances of a list of input trees. Use given distance
-- measure. Returns a triple, the first two elements are the indices of the
-- compared trees, the third is the distance.
pairwise
  :: (a -> a -> b)   -- ^ Distance function
  -> [a]             -- ^ Input trees
  -> [(Int, Int, b)] -- ^ (index i, index j, distance i j)
pairwise dist trs =
  [ (i, j, dist x y)
  | (i : is, x : xs) <- zip (tails [0 ..]) (tails trs)
  , (j     , y     ) <- zip is xs
  ]

-- | Compute distances between adjacent pairs of a list of input trees. Use
-- given distance measure.
adjacent
  :: (Tree a -> Tree a -> b) -- ^ Distance function
  -> [Tree a]                -- ^ Input trees
  -> [b]
adjacent dist trs = [ dist x y | (x, y) <- zip trs (tail trs) ]
