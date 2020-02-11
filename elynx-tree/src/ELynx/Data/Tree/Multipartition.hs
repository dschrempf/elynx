{- |
Module      :  ELynx.Data.Tree.Multipartition
Description :  Multipartitions on rose trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Dec 12 12:58:49 2019.

A multifurcation induces a 'Multipartition', similar to branches inducing
'ELynx.Data.Tree.Bipartition's.

-}

module ELynx.Data.Tree.Multipartition
        (
          -- * The 'Multipartition' data type.
          Multipartition()
        , mps
        , mp
        , mpmap
        , mphuman
        , fromBipartition
        -- * Working with 'Multipartition's.
        , multipartitions
        , findSubset
        , compatible
        )
where

import           Data.List                      ( filter
                                                , find
                                                , foldl'
                                                , intercalate
                                                )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S
import           Data.Tree

import           ELynx.Data.Tree.Bipartition    ( Bipartition
                                                , bps
                                                , sshow
                                                )
import           ELynx.Data.Tree.Tree

-- | Each branch of a bifurcating tree partitions the leaves of the tree into
-- three subsets, see 'ELynx.Data.Tree.Bipartition'. In a similar way, each
-- internal node induces a tripartition. Tripartitions are not yet implemented
-- (December 2019) because it is usually sufficient to work with bipartitions.
-- If, however, the tree is multifurcating and a specific node has more than two
-- children, the number of subsets induced by this node is larger than three,
-- and a 'Multipartition'. Multipartitions are interesting in that we can use
-- them for calculating incompatible splits, see 'ELynx.Data.Tree.Distance'. The
-- order of the partitions within a multipartition is unimportant, .
newtype Multipartition a = Multipartition { mps :: S.Set (S.Set a) -- ^ Set of partitions
                                          }
  deriving (Show, Read)

-- | Show a multipartition in a human readable form. Use a provided function to
-- extract the valuable information.
mphuman :: (a -> String) -> Multipartition a -> String
mphuman f (Multipartition xs) =
        "(" ++ intercalate "|" (map (sshow f) (S.toList xs)) ++ ")"

-- | Create a multipartition.
mp :: Ord a => [S.Set a] -> Multipartition a
mp = mp' . filter (not . S.null)

mp' :: Ord a => [S.Set a] -> Multipartition a
mp' xs = Multipartition (S.fromList xs)

-- | Map a function over all elements in the multipartitions.
mpmap :: (Ord a, Ord b) => (a -> b) -> Multipartition a -> Multipartition b
mpmap f (Multipartition xs) = Multipartition $ S.map (S.map f) xs

instance (Eq a) => Eq (Multipartition a) where
        Multipartition xs == Multipartition ys = xs == ys

instance (Ord a) => Ord (Multipartition a) where
        Multipartition xs `compare` Multipartition ys = xs `compare` ys

-- | Convert bipartition to multipartition.
fromBipartition :: Ord a => Bipartition a -> Multipartition a
fromBipartition bp = mp [l, r] where (l, r) = bps bp

-- | Get all multipartitions of a tree.
multipartitions :: Ord a => Tree a -> S.Set (Multipartition a)
multipartitions t = if S.size (S.fromList lvs) == length lvs
        then multipartitionsUnsafe S.empty (partitionTree t)
        else error "multipartitions: The tree contains duplicate leaves."
        where lvs = leaves t

-- | See 'multipartitions', but do not check if leaves are unique.
multipartitionsUnsafe
        :: Ord a => S.Set a -> Tree (S.Set a) -> S.Set (Multipartition a)
multipartitionsUnsafe _  (Node _ []    ) = S.empty
multipartitionsUnsafe xs (Node _ [x]   ) = multipartitionsUnsafe xs x
multipartitionsUnsafe xs (Node _ [x, y]) = S.union l r
    where
        l = multipartitionsUnsafe (S.union xs (rootLabel x)) y
        r = multipartitionsUnsafe (S.union xs (rootLabel y)) x
multipartitionsUnsafe xs t@(Node _ ys) =
        S.unions
                $ S.singleton (mp (xs : map rootLabel ys))
                : zipWith multipartitionsUnsafe lvsOthers ys
        where lvsOthers = subForestGetSubsets xs t

-- | Find the subset of a multipartition containing a given element.
findSubset :: Ord a => a -> Multipartition a -> S.Set a
findSubset l m = -- Return the empty subset if nothing is found. This corresponds
                 -- to having no information about the leaf in question.
                 fromMaybe S.empty (find (S.member l) ss)
        where ss = mps m

-- Add the subset of a bipartition which contains a given element.
addSubset
        :: Ord a => Multipartition a -> S.Set (S.Set a) -> a -> S.Set (S.Set a)
addSubset m ss l = if not $ S.null s then s `S.insert` ss else ss
        where s = findSubset l m

-- Each subset overlaps with a number of subsets of a bipartition which are
-- returned by this function.
overlap :: Ord a => Multipartition a -> S.Set a -> S.Set (S.Set a)
overlap m = foldl' (addSubset m) S.empty

-- | Multipartitions are compatible if they do not contain conflicting
-- information. This function checks if two multipartitions are compatible with
-- each other. Thereby, following algorithm is used:
--
-- 1. Take each subset of the first multipartition.
--
-- 2a. Determine the overlap: For each leaf of the chosen subset, add the subset
-- of the second multipartition containing the leaf. The result is a set of
-- subsets, which is the union of the added subsets.
--
-- The data type "set of subsets" is actually the same data type as a
-- multipartition. However, it is not a partition, because it may and will not
-- span the whole set of leaves, and so, I use @S.Set (S.Set a)@. One could
-- define a multiset data type to improve comprehensibility.
--
-- 2b. Collect the set of subsets from point 1.
--
-- 3. Each set of subsets needs to be either equal or disjoint with any other
-- set of subsets in the collection. If so, the first multipartition is
-- compatible with the second.
--
-- 4. Exchange the first with the second multipartition and go through steps 1
-- to 3.
--
-- See also 'ELynx.Data.Tree.Bipartition.compatible'.
compatible :: (Ord a, Show a) => Multipartition a -> Multipartition a -> Bool
compatible l r =
        and
                $  [ x `S.disjoint` y | x <- lOverlaps, y <- lOverlaps, x /= y ]
                ++ [ x `S.disjoint` y | x <- rOverlaps, y <- rOverlaps, x /= y ]
    where
        ls        = S.toList $ mps l
        rs        = S.toList $ mps r
        -- The subsets on the left multipartition overlap the subsets of the
        -- right multipartition.
        lOverlaps = map (overlap r) ls
        -- The subsets on the left multipartition overlap the subsets of the
        -- right multipartition.
        rOverlaps = map (overlap l) rs
