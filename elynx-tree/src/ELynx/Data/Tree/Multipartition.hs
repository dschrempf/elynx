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
  ( -- * The 'Multipartition' data type.
    Multipartition ()
  , mps
  , mp
  , mpmap
  , mphuman
    -- * Working with 'Multipartition's.
  , multipartitions
  , findMp
  , compatible
  ) where

import           Data.List                (filter, find, foldl', intercalate,
                                           sort)
import           Data.Maybe               (fromMaybe)
import qualified Data.Set                 as S
import           Data.Tree

import           ELynx.Data.Tree.Subgroup
import           ELynx.Data.Tree.Tree

-- | Each branch of a bifurcating tree partitions the leaves of the tree into
-- three 'Subgroup's, see 'ELynx.Data.Tree.Bipartition'. In a similar way, each
-- internal node induces a tripartition. Tripartitions are not yet implemented
-- (December 2019) because it is usually sufficient to work with bipartitions.
-- If, however, the tree is multifurcating and a specific node has more than two
-- children, the number of subgroups induced by this node is larger than three,
-- and a 'Multipartition'. Multipartitions are interesting in that we can use
-- them for calculating incompatible splits, see 'ELynx.Data.Tree.Distance'. The
-- order of the partitions within a multipartition is unimportant, .
newtype Multipartition a = Multipartition { mps :: [Subgroup a] -- ^ List of partitions
                                          }
  deriving (Show, Read)

-- | Show a multipartition in a human readable form. Use a provided function to
-- extract the valuable information.
mphuman :: (a -> String) -> Multipartition a -> String
mphuman f (Multipartition xs) = "(" ++ intercalate "|" (map (sshow f) xs) ++  ")"

-- | Create a multipartition.
mp :: Ord a => [Subgroup a] -> Multipartition a
mp = mp' . filter (not . snull)

mp' :: Ord a => [Subgroup a] -> Multipartition a
-- XXX: For now also allow multipartitions with no, one, or two elements.
-- mp' []     = error "mp': Cannot create multipartition from empty list."
-- mp' [_]    = error "mp': Cannot create multipartition from list with one element."
-- mp' [_, _] = error "mp': Cannot create multipartition from list with two elements."
mp' xs     = Multipartition (sort xs)

-- | Map a function over all elements in the multipartitions.
mpmap :: (Ord a, Ord b) => (a -> b) -> Multipartition a -> Multipartition b
mpmap f (Multipartition xs) = mp $ map (smap f) xs

instance (Eq a) => Eq (Multipartition a) where
  Multipartition xs == Multipartition ys = xs == ys

instance (Ord a) => Ord (Multipartition a) where
  Multipartition xs `compare` Multipartition ys = xs `compare` ys

-- | Get all multipartitions of a tree.
multipartitions :: Ord a => Tree a -> S.Set (Multipartition a)
multipartitions t = if S.size (S.fromList lvs) == length lvs
                    then multipartitionsUnsafe sempty (partitionTree t)
                    else error "multipartitions: The tree contains duplicate leaves."
  where lvs = leaves t

-- | See 'multipartitions', but do not check if leaves are unique.
multipartitionsUnsafe :: Ord a => Subgroup a -> Tree (Subgroup a) -> S.Set (Multipartition a)
multipartitionsUnsafe _    (Node _ []    ) = S.empty
multipartitionsUnsafe xs   (Node _ [x]   ) = multipartitionsUnsafe xs x
multipartitionsUnsafe xs   (Node _ [x, y]) = S.union l r
  where l = multipartitionsUnsafe (sunion xs (rootLabel x)) y
        r = multipartitionsUnsafe (sunion xs (rootLabel y)) x
multipartitionsUnsafe xs t@(Node _ ys    ) = S.unions $
  S.singleton (mp (xs : map rootLabel ys))
  : zipWith multipartitionsUnsafe lvsOthers ys
  where lvsOthers = subForestGetSubgroups xs t

-- | Find the multipartition containing a given element.
findMp :: Ord a => a -> Multipartition a -> Subgroup a
findMp l m = fromMaybe
             -- Return the empty subgroup if nothing is found. This corresponds
             -- to having no information about the leaf in question.
             sempty
             (find (smember l) ps)
  where ps = mps m

takeLeaf :: Ord a => Multipartition a -> Subgroup a -> a -> Subgroup a
takeLeaf m p l | l `smember` p = p
               | otherwise     = p `sunion` findMp l m

takeLeaves :: Ord a => Subgroup a -> Multipartition a -> Subgroup a
takeLeaves p m = foldl' (takeLeaf m) sempty p

-- | Check if a 'Subgroup' is compatible with a 'Multipartition'.
compatible :: (Ord a) => Subgroup a -> Multipartition a -> Bool
compatible s m = (takeLeaves s m `sdifference` s) == sempty
