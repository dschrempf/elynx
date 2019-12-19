{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  ELynx.Data.Tree.Subgroup
Description :  A subgroup is a set of elements
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Dec 13 11:02:43 2019.

-}

module ELynx.Data.Tree.Subgroup
  ( Subgroup ()
  , sfromset
  , sfromlist
  , smap
  , snull
  , sempty
  , ssingleton
  , sunion
  , sunions
  , sdifference
  , smember
  , sshow
  ) where

import           Data.List (intercalate)
import qualified Data.Set  as S

-- | A 'Subgroup' is a set of elements of type a. For example, on phylogenetic
-- trees, a 'Subgroup' is a set of leaves. In this case, a 'Subgroup' is
-- induced, for example, by a node on the (rooted) tree. The 'Subgroup's of leaf
-- nodes are singletons. The 'Subgroup's of the root node is the set of all
-- leaves. Each branch on the tree induces a pair of 'Subgroup's, see
-- 'ELynx.Data.Tree.Bipartition'. Multifurcations induce multipartitions, see
-- 'ELynx.Data.Tree.Multipartition'.
--
-- Internally, a subgroup is just an 'S.Set', since the order of elements
-- within the subgroup is not important, but the uniqueness of elements is.
newtype Subgroup a = SG {subgroup :: S.Set a}
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid, Foldable)

-- | Create a subgroup from a set.
sfromset :: S.Set a -> Subgroup a
sfromset = SG

-- | Create a subgroup from a list. Throws an error if duplicate elements are
-- present in the list.
sfromlist :: Ord a => [a] -> Subgroup a
sfromlist l = if S.size s == length l
              then sfromset s
              else error "pfromlist: List contains duplicate elements."
  where s = S.fromList l

-- | Map a function over all elements in a subgroup.
smap :: Ord b => (a -> b) -> Subgroup a -> Subgroup b
smap f = SG . S.map f . subgroup

-- | Is the subgroup empty?
snull :: Subgroup a -> Bool
snull = S.null . subgroup

-- | The empty subgroup.
sempty :: Subgroup a
sempty = SG S.empty

-- | A subgroup with one element.
ssingleton :: a -> Subgroup a
ssingleton = SG . S.singleton

-- | Unite two subgroups.
sunion :: Ord a => Subgroup a -> Subgroup a -> Subgroup a
sunion p q = SG $ subgroup q `S.union` subgroup p

-- | Unite a list of subgroups.
sunions :: Ord a => [Subgroup a] -> Subgroup a
sunions = SG . S.unions . map subgroup

-- | Difference of two subgroups.
sdifference :: Ord a => Subgroup a -> Subgroup a -> Subgroup a
sdifference p q = SG $ subgroup p S.\\ subgroup q

-- | Check if an element is member of a subgroup.
smember :: Ord a => a -> Subgroup a -> Bool
smember x = S.member x . subgroup

-- | Show the elements of a subgroup in a human readable way.
sshow :: (a -> String) -> Subgroup a -> String
sshow f = intercalate "," . map f . S.toList . subgroup
