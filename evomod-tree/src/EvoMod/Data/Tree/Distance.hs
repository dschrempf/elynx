{- |
Module      :  EvoMod.Data.Tree.Distance
Description :  Compute distances between trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jun 13 17:15:54 2019.

TODO: Compare with http://evolution.genetics.washington.edu/phylip/doc/treedist.html.

TODO: Implement branch score distance.

-}

module EvoMod.Data.Tree.Distance
  ( bipartitions
  , multipartitions
  , symmetricDistance
  , incompatibleSplitsDistance
  , computePairwiseDistances
  , computeAdjacentDistances
  ) where

import           Data.List
import qualified Data.Set              as Set
import           Data.Tree

import           EvoMod.Data.Tree.Tree

leavesSet :: Ord a => Tree a -> Set.Set a
leavesSet = Set.fromList . leaves

-- TODO.
-- -- | Each node of a tree is root of a subtree. Set the node label to the leaves
-- -- of this subtree.
-- toLeavesTree :: Tree a -> Tree [a]
-- toLeavesTree (Node l []) = Node [l] []
-- toLeavesTree (Node _ xs) = Node (concatMap rootLabel xs') xs'
--   where xs' = map toLeavesTree xs

-- | Bipartitions with 'Set.Set's, since order of elements within the leaf sets
-- is not important. Also the order of the two leaf sets of the bipartition is
-- not important (see 'Eq' instance definition).
newtype Bipartition a = Bipartition (Set.Set a, Set.Set a)

instance (Show a, Ord a) => Show (Bipartition a) where
  show (Bipartition (x, y)) = if x >= y then show (x, y) else show (y, x)

instance (Eq a) => Eq (Bipartition a) where
  Bipartition (x,y) == Bipartition (v, w) = ((x == v) && (y == w)) || ((x == w) && (y == v))

-- | Get all bipartitions. XXX: This is slow at the moment, because 'leaves' is
-- called excessively.
bipartitions :: Ord a => Tree a -> [Bipartition a]
bipartitions = bipartitions' Set.empty

-- XXX: A helper function could reduce redundancy a lot in the next functions.
-- bipartitionsThisNode :: Tree a -> [Bipartition a]
-- But:
-- 1. The calling function need to pass on the leaves of the other branches, and
--    so, they have to be recalculated.
-- 2. The unnecessary recalculation of leaves is fostered.
-- XXX: Use 'toLeaves'.
bipartitions' :: Ord a => Set.Set a -> Tree a -> [Bipartition a]
bipartitions' _   (Node _ []    ) = []
bipartitions' lsC (Node _ [c]   ) = bipartitions' lsC c
bipartitions' lsC (Node _ xs    )
  -- It really sucks that we have to treat a bifurcating root separately. But
  -- that's just how it is.
  | Set.null lsC && length xs == 2 =
    let l = head xs
        r = xs !! 1
        lsL = leavesSet l
        lsR = leavesSet r
    in Bipartition (lsL, lsR) : bipartitions' lsL r ++ bipartitions' lsR l
  | otherwise = bs ++ concat (zipWith bipartitions' lsOthers xs)
  where
    nChildren  = length xs
    lsChildren = map leavesSet xs
    lsOthers   = [ Set.unions $ lsC : take i lsChildren ++ drop (i+1) lsChildren
                      | i <- [0 .. (nChildren - 1)] ]
    bs         = map Bipartition $ zip lsChildren lsOthers

-- XXX: Rename this function. It does not compute multipartitions, rather it
-- computes bipartitions, but merges leaves for multifurcations.
-- | Get all bipartitions, but combine leaves from multi-furcations. This is
-- useful to find incompatible splits. See 'incompatibleSplitsDistance'.
multipartitions :: Ord a => Tree a -> [Bipartition a]
-- Assume that a root node with three children actually corresponds to an
-- unrooted tree.
multipartitions (Node _ [a, b, c]) = Bipartition (lsA, lsBC)
                                     : Bipartition (lsB, lsAC)
                                     : Bipartition (lsC, lsAB)
                                     : multipartitions' lsBC a
                                     ++ multipartitions' lsAC b
                                     ++ multipartitions' lsAB c
  where
    lsA = leavesSet a
    lsB = leavesSet b
    lsC = leavesSet c
    lsAB = lsA `Set.union` lsB
    lsAC = lsA `Set.union` lsC
    lsBC = lsB `Set.union` lsC
multipartitions n                  = multipartitions' Set.empty n

multipartitions' :: Ord a => Set.Set a -> Tree a -> [Bipartition a]
multipartitions' _   (Node _ []    ) = []
multipartitions' lsC (Node _ [c]   ) = multipartitions' lsC c
multipartitions' lsC (Node _ [l, r])
  | Set.null lsC = let lsL = leavesSet l
                       lsR = leavesSet r
                   in Bipartition (lsL, lsR) : multipartitions' lsL r ++ multipartitions' lsR l
  | otherwise = let lsL = leavesSet l
                    lsR = leavesSet r
                    lsCL = lsL `Set.union` lsC
                    lsCR = lsR `Set.union` lsC
                in Bipartition (lsCL, lsR) : Bipartition (lsCR, lsL) :
                   multipartitions' lsCL r ++ multipartitions' lsCR l
multipartitions' lsC n
  | Set.null lsC = []
  | otherwise = map Bipartition [(lsC, leavesSet n)]

symmetricDifference :: Eq a => [a] -> [a] -> Int
symmetricDifference xs ys = length xsNotInYs + length ysNotInXs
  where xsNotInYs = filter (`notElem` ys) xs
        ysNotInXs = filter (`notElem` xs) ys

-- | Symmetric (Robinson-Foulds) distance between two trees. Assumes that the
-- leaves have unique names! XXX: Comparing a list of trees with this function
-- recomputes bipartitions.
symmetricDistance :: (Ord a, Eq a) => Tree a -> Tree a -> Int
symmetricDistance t1 t2 = symmetricDifference (bipartitions t1) (bipartitions t2)

-- | Number of incompatible splits. Similar to 'symmetricDistance' but merges
-- multifurcations.
incompatibleSplitsDistance :: (Ord a, Eq a) => Tree a -> Tree a -> Int
incompatibleSplitsDistance t1 t2 = symmetricDifference (multipartitions t1) (multipartitions t2)

-- | Compute pairwise distances of a list of input trees. Use given distance
-- measure function. Returns a triple, the first two elements are the indices of
-- the compared trees, the third is the distance.
computePairwiseDistances :: (Tree a -> Tree a -> Int) -- ^ Distance function
                         -> [Tree a]                  -- ^ Input trees
                         -> [(Int, Int, Int)]         -- ^ (index i, index j, distance i j)
computePairwiseDistances dist trs = [ (i, j, dist x y)
                                    | (i:is, x:xs) <- zip (tails [0..]) (tails trs)
                                    , (j, y) <- zip is xs ]

-- | Compute distances between adjacent pairs of a list of input trees. Use
-- given distance measure function.
computeAdjacentDistances :: (Tree a -> Tree a -> Int) -- ^ Distance function
                         -> [Tree a]                  -- ^ Input trees
                         -> [Int]
computeAdjacentDistances dist trs = [ dist x y | (x, y) <- zip trs (tail trs) ]
