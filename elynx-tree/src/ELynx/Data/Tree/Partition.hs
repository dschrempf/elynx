{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  ELynx.Data.Tree.Partition
Description :  A partition is a set of elements
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Dec 13 11:02:43 2019.

-}

module ELynx.Data.Tree.Partition
  ( Partition ()
  , pfromset
  , pfromlist
  , pmap
  , pnull
  , pempty
  , psingleton
  , punion
  , punions
  , pdifference
  , pmember
  , pshow
  ) where

import           Data.List (intercalate)
import qualified Data.Set  as S

-- | A partition is a set of elements of type a. For example, on phylogenetic
-- trees, a partition is a set of leaves. Then, a partition is induced by a node
-- on the (rooted) tree. The partitions of leave nodes are singletons. The
-- partitions of the root node is the set of all leaves. Each branch on the tree
-- induces a pair of partitions, see 'ELynx.Data.Tree.Bipartition'.
-- Multifurcations induce multipartitions, see 'ELynx.Data.Tree.Multipartition'.
--
-- Internally, a partition is just an 'S.Set', since the order of elements
-- within the partition is not important, but the uniqueness of elements is.
newtype Partition a = P {partition :: S.Set a}
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid, Foldable)

-- | Create a partition from a set.
pfromset :: S.Set a -> Partition a
pfromset = P

-- | Create a partition from a list. Throws an error if duplicate elements are
-- present in the list.
pfromlist :: Ord a => [a] -> Partition a
pfromlist l = if S.size s == length l
              then pfromset s
              else error "pfromlist: List contains duplicate elements."
  where s = S.fromList l


-- | Map a function over all elements in a partition.
pmap :: Ord b => (a -> b) -> Partition a -> Partition b
pmap f = P . S.map f . partition

-- | Is the partition empty?
pnull :: Partition a -> Bool
pnull = S.null . partition

-- | The empty partition.
pempty :: Partition a
pempty = P S.empty

-- | A partition with one element.
psingleton :: a -> Partition a
psingleton = P . S.singleton

-- | Unite two partitions.
punion :: Ord a => Partition a -> Partition a -> Partition a
punion p q = P $ partition q `S.union` partition p

-- | Unite a list of partitions.
punions :: Ord a => [Partition a] -> Partition a
punions = P . S.unions . map partition

-- | Difference of two partitions.
pdifference :: Ord a => Partition a -> Partition a -> Partition a
pdifference p q = P $ partition p S.\\ partition q

-- | Check if an element is member of a partition.
pmember :: Ord a => a -> Partition a -> Bool
pmember x = S.member x . partition

-- | Show the elements of a partition in a human readable way.
pshow :: (a -> String) -> Partition a -> String
pshow f = intercalate "," . map f . S.toList . partition
