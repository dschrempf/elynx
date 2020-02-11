{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  ELynx.Data.Tree.Subset
Description :  A subset is a set of elements
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Dec 13 11:02:43 2019.

TODO: Check that this module is used consistently.

-}

module ELynx.Data.Tree.Subset
  ( Subset ()
  , sfromset
  , sfromlist
  , smap
  , snull
  , sempty
  , ssingleton
  , sunion
  , sunions
  , sdifference
  , sintersection
  , sdisjoint
  , smember
  , sshow
  ) where

import           Data.List (intercalate)
import qualified Data.Set  as S

-- | A 'Subset' is a set of elements of type a. For example, on phylogenetic
-- trees, a 'Subset' is a set of leaves. In this case, a 'Subset' is induced,
-- for example, by a node on the (rooted) tree. The 'Subset's of leaf nodes are
-- singletons. The 'Subset's of the root node is the set of all leaves.
-- 'Subset's are the building blocks of partitions. Each branch on the tree
-- induces a bipartition, or a pair of 'Subset's, see
-- 'ELynx.Data.Tree.Bipartition'. Multifurcations induce multipartitions, see
-- 'ELynx.Data.Tree.Multipartition'.
--
-- Internally, a subset is just an 'S.Set', since the order of elements
-- within the subset is not important, but the uniqueness of elements is.
newtype Subset a = SS {subset :: S.Set a}
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid, Foldable)

-- | Create a subset from a set.
sfromset :: S.Set a -> Subset a
sfromset = SS

-- | Create a subset from a list. Throws an error if duplicate elements are
-- present in the list.
sfromlist :: Ord a => [a] -> Subset a
sfromlist l =
  if S.size s == length l
  then sfromset s
  else error "sfromlist: List contains duplicate elements."
  where s = S.fromList l

-- | Map a function over all elements in a subset.
smap :: Ord b => (a -> b) -> Subset a -> Subset b
smap f = SS . S.map f . subset

-- | Is the subset empty?
snull :: Subset a -> Bool
snull = S.null . subset

-- | The empty subset.
sempty :: Subset a
sempty = SS S.empty

-- | A subset with one element.
ssingleton :: a -> Subset a
ssingleton = SS . S.singleton

-- | Unite two subsets.
sunion :: Ord a => Subset a -> Subset a -> Subset a
sunion (SS p) (SS q) =
  if S.disjoint p q
  then SS $ q `S.union` p
  else error "sunion: lists contain duplicate elements."

-- | Unite a list of subsets.
sunions :: Ord a => [Subset a] -> Subset a
sunions ps =
  if S.size res == sum (map length ps)
  then SS res
  else error "sunions: list contains subsets with duplicate elements."
  where res = S.unions . map subset $ ps

-- | Difference of two subsets.
sdifference :: Ord a => Subset a -> Subset a -> Subset a
sdifference p q = SS $ subset p S.\\ subset q

-- | Intersection of two subsets.
sintersection :: Ord a => Subset a -> Subset a -> Subset a
sintersection p q = SS $ S.intersection (subset p) (subset q)

-- | Are two subsets disjoint?
sdisjoint :: Ord a => Subset a -> Subset a -> Bool
sdisjoint p q = S.disjoint (subset p) (subset q)

-- | Check if an element is member of a subset.
smember :: Ord a => a -> Subset a -> Bool
smember x = S.member x . subset

-- | Show the elements of a subset in a human readable way.
sshow :: (a -> String) -> Subset a -> String
sshow f = intercalate "," . map f . S.toList . subset
