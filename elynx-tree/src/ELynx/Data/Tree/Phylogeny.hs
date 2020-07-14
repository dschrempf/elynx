{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Data.Tree.Phylogeny
-- Description :  Phylogenetic trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 16:08:54 2019.
--
-- A phylogeny is a 'Tree' with branch and node labels. The node labels are
-- unique, and the order of the trees in the sub-forest is meaningless.
--
-- Internally, however, the underlying 'Tree' data structure stores the
-- sub-forest as a list, which has a specific order. Hence, we have to do some
-- tricks when comparing trees, and tree comparison is slow.
--
-- Also, the uniqueness of the leaves is not ensured by the data type, but has
-- to be checked at runtime.
--
-- NOTE: Trees in this library are all rooted.
module ELynx.Data.Tree.Phylogeny
  ( equal,
    equalTopology,
    valid,
    Phylo (..),
    Length (..),
    phyloToLengthTree,
    lengthToPhyloTree,
    Support (..),
    phyloToSupportTree,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as S
import ELynx.Data.Tree.Measurable
import ELynx.Data.Tree.Rooted
import ELynx.Data.Tree.Supported

-- | The equality check is slow because the order of children is arbitrary.
equal :: (Eq e, Eq a) => Tree e a -> Tree e a -> Bool
equal ~(Node brL lbL tsL) ~(Node brR lbR tsR) =
  (brL == brR)
    && (lbL == lbR)
    && (length tsL == length tsR)
    && all (`elem` tsR) tsL

-- | Check if two trees have the same topology.
equalTopology :: Eq a => Tree e a -> Tree e a -> Bool
equalTopology l r = rmBr l == rmBr r
  where
    rmBr = first (const ())

hasNoDuplicates :: Ord a => [a] -> Bool
hasNoDuplicates = go S.empty
  where
    go _ [] = True
    go seen (x : xs) = x `S.notMember` seen && go (S.insert x seen) xs

-- | Check if a tree is valid, that is, if the leaves are unique.
valid :: Ord a => Tree e a -> Bool
valid = hasNoDuplicates . leaves

-- | Branch label for phylogenetic trees.
--
-- Branches may have a length and a support value.
data Phylo = Phylo
  { brLen :: Maybe BranchLength,
    brSup :: Maybe BranchSupport
  }
  deriving (Read, Show, Eq, Ord)

-- | Branch length label. For conversion, see 'phyloToLengthTree' and 'lengthToPhyloTree'.
newtype Length = Length {fromLength :: BranchLength}
  deriving (Read, Show, Eq, Ord, Num, Fractional, Floating)
  deriving (Monoid) via Sum Double
  deriving (Semigroup) via Sum Double

instance Measurable Length where
  getLen = fromLength
  setLen b _ = Length b

-- | If root branch length is not available, set it to 0. Return 'Nothing' if
-- any other branch length is unavailable.
phyloToLengthTree :: Tree Phylo a -> Either String (Tree Length a)
phyloToLengthTree =
  maybe (Left "phyloToLengthTree: Length unavailable for some branches.") Right
    . bitraverse toLength pure
    . cleanRootLength

cleanRootLength :: Tree Phylo a -> Tree Phylo a
cleanRootLength (Node (Phylo Nothing s) l f) = Node (Phylo (Just 0) s) l f
cleanRootLength t = t

toLength :: Phylo -> Maybe Length
toLength p = Length <$> brLen p

-- | Set all branch support values to 'Nothing'. Useful, for example, to export
-- a tree with branch lengths in Newick format.
lengthToPhyloTree :: Tree Length a -> Tree Phylo a
lengthToPhyloTree = first fromLengthLabel

fromLengthLabel :: Length -> Phylo
fromLengthLabel (Length b) = Phylo (Just b) Nothing

-- | Branch support label. For conversion, see 'phyloToSupportTree'.
newtype Support = Support {fromSupport :: BranchSupport}
  deriving (Read, Show, Eq, Ord, Num, Fractional)

instance Supported Support where
  getBranchSupport = fromSupport
  setBranchSupport s _ = Support s

-- | Set branch support values of branches leading to the leaves and of the root
-- branch to maximum support.
--
-- Return 'Nothing' if any other branch has no available support value.
phyloToSupportTree :: Tree Phylo a -> Either String (Tree Support a)
phyloToSupportTree t =
  maybe
    (Left "phyloToSupportTree: Support unavailable for some branches.")
    Right
    $ bitraverse toSupport pure $ cleanLeafSupport m $ cleanRootSupport m t
  where
    m = getMaxSupport t

-- If all branch support values are below 1.0, set the max support to 1.0.
getMaxSupport :: Tree Phylo a -> BranchSupport
getMaxSupport = fromJust . max (Just 1.0) . bimaximum . bimap brSup (const Nothing)

cleanRootSupport :: BranchSupport -> Tree Phylo a -> Tree Phylo a
cleanRootSupport maxSup (Node (Phylo b Nothing) l xs) = Node (Phylo b (Just maxSup)) l xs
cleanRootSupport _ t = t

cleanLeafSupport :: BranchSupport -> Tree Phylo a -> Tree Phylo a
cleanLeafSupport s (Node (Phylo b Nothing) l []) = Node (Phylo b (Just s)) l []
cleanLeafSupport s (Node b l xs) = Node b l $ map (cleanLeafSupport s) xs

toSupport :: Phylo -> Maybe Support
toSupport (Phylo _ Nothing) = Nothing
toSupport (Phylo _ (Just s)) = Just $ Support s

-- TODO: Something like 'PhyloStrict' with conversion functions.
