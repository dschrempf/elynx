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
    mpUnsafe,
    bpToMp,
    mpHuman,
    -- mpMap,

    -- * Work with 'Multipartition's
    multipartitions,
    compatible,
  )
where

-- TODO: Call this Partition, not Multipartition.

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

-- TODO: Check that list is not empty after filtering.

-- | Create a multipartition.
mp :: Ord a => [Set a] -> Either String (Multipartition a)
mp xs = case filter (not . S.null) xs of
  [] -> Left "mp: Empty list."
  xs' -> Right $ mpUnsafe xs'

-- | Create a multipartition.
mpUnsafe :: Ord a => [Set a] -> Multipartition a
mpUnsafe xs = Multipartition (S.fromList xs)

-- | Convert a bipartition to a multipartition.
bpToMp :: Ord a => Bipartition a -> Multipartition a
bpToMp = mpUnsafe . tupleToList . fromBipartition
  where
    -- Be careful with tuples, because 'toList' does something very weird. It only
    -- takes the second element of the tuple!
    --
    -- toList :: Foldable t => t a -> [a]
    tupleToList (x, y) = [x, y]

-- | Show a multipartition in a human readable form. Use a provided function to
-- extract the valuable information.
mpHuman :: Show a => Multipartition a -> String
mpHuman (Multipartition xs) =
  "(" ++ intercalate "|" (map setShow (S.toList xs)) ++ ")"

-- Show the elements of a set in a human readable format.
setShow :: Show a => Set a -> String
setShow = intercalate "," . map show . S.toList

-- -- | Map a function over all elements in the 'Multipartition'.
-- mpMap :: (Ord a, Ord b) => (a -> b) -> Multipartition a -> Multipartition b
-- mpMap f (Multipartition xs) = Multipartition $ S.map (S.map f) xs

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
    either (const S.empty) S.singleton (mp (p : map label ts)) :
    zipWith multipartitions' cs ts
  where
    cs = getComplementaryLeaves p t

-- | 'Multipartition's are compatible if they do not contain conflicting
-- information. This function checks if two multipartitions are compatible with
-- each other. Thereby, a variation of the following algorithm is used:
--
-- @
-- mp1 `compatible` mp2
-- for set1 in mp1:
--   for set2 in mp2:
--     if set1 `S.isSubSetOf` set2:
--       remove set1 from mp1
--     if set2 `S.isSubSetOf` set1:
--       remove set2 from mp2
-- if either mp2 or mp2 is empty, they are compatible
-- @
compatible :: (Show a, Ord a) => Multipartition a -> Multipartition a -> Bool
compatible l r = S.null (S.filter (`remove` rs) ls) || S.null (S.filter (`remove` ls) rs)
  where
    ls = fromMultipartition l
    rs = fromMultipartition r

remove :: Ord a => Set a -> Set (Set a) -> Bool
remove s = not . any (s `S.isSubsetOf`)
