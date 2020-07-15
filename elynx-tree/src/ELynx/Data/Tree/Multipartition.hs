-- |
-- Module      :  ELynx.Data.Tree.Multipartition
-- Description :  Multipartitions on rose trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Dec 12 12:58:49 2019.
--
-- A multifurcation induces a 'Multipartition', similar to branches inducing
-- 'ELynx.Data.Tree.Bipartition's.
module ELynx.Data.Tree.Multipartition
  ( -- * Data type
    Multipartition (fromMultipartition),
    mp,
    bpToMp,
    mpHuman,
    mpMap,

    -- * Work with 'Multipartition's
    multipartitions,
    multipartitionCompatible,
  )
where

import Data.Foldable
import Data.List hiding (partition)
import qualified Data.Set as S
import Data.Set (Set)
import ELynx.Data.Tree.Bipartition
import ELynx.Data.Tree.Phylogeny
import ELynx.Data.Tree.Rooted

-- | Each branch of a tree partitions the leaves of the tree into two subsets
-- (see 'ELynx.Data.Tree.Bipartition'). In a similar way, each internal node
-- (excluding the root node) partitions the leaves into three (or more) subsets
-- which is called 'Multipartition'. If the tree is multifurcating, and a
-- specific node has more than two children, the number of subsets induced by
-- this node is larger than three. Multipartitions are interesting in that we
-- can use them for calculating incompatible splits, see
-- 'ELynx.Data.Tree.Distance'.
--
-- The order of the subsets of a 'Multipartition' is meaningless. We ensure by
-- construction that the subsets are ordered, and hence, that equality checks
-- are meaningful.
newtype Multipartition a = Multipartition
  { fromMultipartition :: Set (Set a)
  }
  deriving (Eq, Ord, Show, Read)

-- | Create a multipartition.
mp :: Ord a => [Set a] -> Multipartition a
mp = mp' . filter (not . S.null)

-- Unsafe.
mp' :: Ord a => [Set a] -> Multipartition a
mp' xs = Multipartition (S.fromList xs)

-- | Convert a bipartition to a multipartition.
bpToMp :: Ord a => Bipartition a -> Multipartition a
bpToMp = mp . toList . fromBipartition

-- | Show a multipartition in a human readable form. Use a provided function to
-- extract the valuable information.
mpHuman :: Show a => Multipartition a -> String
mpHuman (Multipartition xs) =
  "(" ++ intercalate "|" (map setShow (S.toList xs)) ++ ")"

-- Show the elements of a set in a human readable format.
setShow :: Show a => Set a -> String
setShow = intercalate "," . map show . S.toList

-- | Map a function over all elements in the 'Multipartition'.
mpMap :: (Ord a, Ord b) => (a -> b) -> Multipartition a -> Multipartition b
mpMap f (Multipartition xs) = Multipartition $ S.map (S.map f) xs

-- | Get all 'Multipartition's of a tree.
multipartitions :: Ord a => Tree e a -> Either String (Set (Multipartition a))
multipartitions t
  | valid t = Right $ multipartitions' S.empty $ S.fromList <$> groups t
  | otherwise = Left "multipartitions: Tree contains duplicate leaves."

-- See 'multipartitions', but do not check if leaves are unique.
multipartitions' :: Ord a => Set a -> Tree e (Set a) -> Set (Multipartition a)
multipartitions' _ (Node _ _ []) = S.empty
multipartitions' p t@(Node _ _ ts) =
  S.unions $
    S.singleton (mp (p : map label ts)) :
    zipWith multipartitions' cs ts
  where
    cs = getComplementaryLeaves p t

-- Find the subset of a multipartition containing a given element.
findSubset :: Ord a => a -> Multipartition a -> Maybe (Set a)
findSubset l = find (S.member l) . fromMultipartition

-- Find and collect the subset of a multipartition which contains a given element.
addSubset :: Ord a => Multipartition a -> Set (Set a) -> a -> Set (Set a)
addSubset m acc l = case findSubset l m of
  Just s -> s `S.insert` acc
  Nothing -> acc

-- Return the subsets of the multipartition overlapping with the given set.
overlap :: Ord a => Multipartition a -> Set a -> Set (Set a)
overlap m = foldl' (addSubset m) S.empty

-- | 'Multipartition's are compatible if they do not contain conflicting
-- information. This function checks if two multipartitions are compatible with
-- each other. Thereby, the following algorithm is used:
--
-- 1. Take each subset of the first multipartition.
--
-- 2. Determine the overlap: For each leaf of the chosen subset, add the subset
--    of the second multipartition containing the leaf. The result is a set of
--    subsets, which is the union of the added subsets.
--
--    The data type "set of subsets" is actually the same data type as a
--    multipartition. However, it is not a partition, because it may and will not
--    span the whole set of leaves, and so, I use @Set (Set a)@. One could define
--    a multiset data type to improve comprehensibility.
--
-- 3. Collect the set of subsets from point 2.
--
-- 4. Each set of subsets needs to be either equal or disjoint with any other
--    set of subsets in the collection. If so, the first multipartition is
--    compatible with the second.
--
-- 5. Exchange the first with the second multipartition and go through steps 1
--    to 4.
--
-- See also 'ELynx.Data.Tree.Bipartition.compatible'.
multipartitionCompatible :: Ord a => Multipartition a -> Multipartition a -> Bool
multipartitionCompatible l r =
  and $
    [x `S.disjoint` y | x <- lOverlaps, y <- lOverlaps, x /= y]
      ++ [x `S.disjoint` y | x <- rOverlaps, y <- rOverlaps, x /= y]
  where
    ls = S.toList $ fromMultipartition l
    rs = S.toList $ fromMultipartition r
    -- The subsets on the left multipartition overlap the subsets of the
    -- right multipartition.
    lOverlaps = map (overlap r) ls
    -- The subsets on the left multipartition overlap the subsets of the
    -- right multipartition.
    rOverlaps = map (overlap l) rs
