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
  ) where

import           Data.List                   (filter, find, intercalate, sort)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as S
import           Data.Tree

import           ELynx.Data.Tree.Partition
import           ELynx.Data.Tree.Tree

-- | Each node of a tree partitions the leaves of the tree into three
-- 'Partition's. If a node has more than two children, the number of induced
-- partitions is larger than three, and a 'Multipartition' is induced. The order
-- of the partitions is unimportant, see also 'ELynx.Data.Tree.Bipartition'.
newtype Multipartition a = Multipartition { mps :: [Partition a] -- ^ List of partitions
                                          }
  deriving (Show, Read)

-- | Show a multipartition in a human readable form. Use a provided function to
-- extract the valuable information.
mphuman :: (a -> String) -> Multipartition a -> String
mphuman f (Multipartition xs) = "(" ++ intercalate "|" (map (pshow f) xs) ++  ")"

-- | Create a multipartition.
mp :: Ord a => [Partition a] -> Multipartition a
mp = mp' . filter (not . pnull)

mp' :: Ord a => [Partition a] -> Multipartition a
mp' []     = error "mp': Cannot create multipartition from empty list."
mp' [_]    = error "mp': Cannot create multipartition from list with one element."
mp' [_, _] = error "mp': Cannot create multipartition from list with two elements."
mp' xs     = Multipartition (sort xs)

-- | Map a function over all elements in the multipartitions.
mpmap :: (Ord a, Ord b) => (a -> b) -> Multipartition a -> Multipartition b
mpmap f (Multipartition xs) = mp $ map (pmap f) xs

instance (Eq a) => Eq (Multipartition a) where
  Multipartition xs == Multipartition ys = xs == ys

instance (Ord a) => Ord (Multipartition a) where
  Multipartition xs `compare` Multipartition ys = xs `compare` ys

-- | Get all multipartitions of a tree.
multipartitions :: Ord a => Tree a -> S.Set (Multipartition a)
multipartitions t = if S.size (S.fromList lvs) == length lvs
                    then multipartitionsUnsafe pempty (partitionTree t)
                    else error "multipartitions: The tree contains duplicate leaves."
  where lvs = leaves t

-- | See 'multipartitions', but do not check if leaves are unique.
multipartitionsUnsafe :: Ord a => Partition a -> Tree (Partition a) -> S.Set (Multipartition a)
multipartitionsUnsafe _    (Node _ []    ) = S.empty
multipartitionsUnsafe xs   (Node _ [x]   ) = multipartitionsUnsafe xs x
multipartitionsUnsafe xs   (Node _ [x, y]) = S.union l r
  where l = multipartitionsUnsafe (punion xs (rootLabel x)) y
        r = multipartitionsUnsafe (punion xs (rootLabel y)) x
multipartitionsUnsafe xs t@(Node _ ys    ) = S.unions $
  S.singleton (mp (xs : map rootLabel ys))
  : zipWith multipartitionsUnsafe lvsOthers ys
  where lvsOthers = subForestGetPartitions xs t

-- | Find the multipartition containing a given element.
findMp :: Ord a => a -> Multipartition a -> Partition a
findMp l m = fromMaybe
             -- Return the empty Partition if nothing is found. This corresponds
             -- to having no information about the leaf in question.
             pempty
             (find (pmember l) ps)
  where ps = mps m
