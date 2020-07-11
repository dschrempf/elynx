-- |
-- Module      :  ELynx.Data.Tree.PhyloTree
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
-- Phylogenetic nodes labels, aka 'PhyloLabel's, have a branch length and an
-- arbitrary label type, e.g., of type 'Int'.
module ELynx.Data.Tree.PhyloTree
  ( PhyloLabel (..),
    Length (..),
    toLength,
    fromLength,
    Support (..),
    toSupport,
  )
where

import Data.Maybe (fromMaybe)
import Data.Tree
import ELynx.Data.Tree.BranchSupportTree
import ELynx.Data.Tree.MeasurableTree
import ELynx.Data.Tree.NamedTree

-- TODO: Rename to Phylo.

-- | A label type for phylogenetic trees possibly with branch length and branch support.
data PhyloLabel a = PhyloLabel
  { pLabel :: a,
    pBrLen :: Maybe BranchLength,
    pBrSup :: Maybe BranchSupport
  }
  deriving (Read, Show, Eq, Ord)

instance Named a => Named (PhyloLabel a) where
  getName = getName . pLabel

-- | A label with a branch length. For conversion, see 'toLength' and
-- 'fromLength'.
data Length a = Length
  { lLabel :: a,
    brLen :: BranchLength
  }
  deriving (Read, Show, Eq, Ord)

instance Named a => Named (Length a) where
  getName = getName . lLabel

instance Measurable (Length a) where
  getLen = brLen
  setLen x l = l {brLen = x}

-- | If root branch length is not available, set it to 0. Return 'Nothing' if
-- any other branch length is unavailable.
toLength :: Tree (PhyloLabel a) -> Maybe (Tree (Length a))
toLength = traverse toLengthLabel . cleanRootLength

cleanRootLength :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
cleanRootLength (Node (PhyloLabel l Nothing s) f) = Node (PhyloLabel l (Just 0) s) f
cleanRootLength _ = id

toLengthLabel :: PhyloLabel a -> Maybe (Length a)
toLengthLabel (PhyloLabel l Nothing _) = Nothing
toLengthLabel (PhyloLabel l (Just b) _) = Length l b

-- | Set all branch support values to 'Nothing'. Useful, for example, to export
-- a tree with branch lengths in Newick format.
fromLength :: Tree (Length a) -> Tree (PhyloLabel a)
fromLength = fmap fromLengthLabel

fromLengthLabel :: Length a -> PhyloLabel a
fromLengthLabel (Length l b) = PhyloLabel l (Just b) Nothing

-- | A label with branch support. For conversion, see 'toSupport'.
data Support a = Support
  { sLabel :: a,
    brSup :: BranchSupport
  }
  deriving (Read, Show, Eq, Ord)

instance Named a => Named (Support a) where
  getName = getName . spLabel

instance BranchSupported (Support a) where
  getBranchSupport = brSup
  setBranchSupport x l = l {brSup = x}

getMaxBrSup :: Tree (PhyloLabel a) -> BranchSupport
getMaxBrSup =
  -- If all branch support values are below 1.0, set the max support to 1.0.
  max 1.0
    .
    -- If no branch support is given, set max support to 1.0.
    fromMaybe 1.0
    . maximum
    . fmap pBrSup

-- | Set branch support values of branches leading to the leaves to maximum
-- support. Set the root branch support to maximum support.
--
-- Return 'Nothing' if any other branch has no available support value.
toSupport :: Tree (PhyloLabel a) -> Maybe (Tree (Support a))
toSupport t = traverse toSupportLabel . cleanLeafSupport m . cleanRootSupport m
  where
    m = getMaxBrSup t

cleanRootSupport :: BranchSupport -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
cleanRootSupport maxSup (Node (PhyloLabel l b Nothing) xs) = Node (PhyloLabel l (Just maxSup)) xs
cleanRootSupport _ t = t

cleanLeafSupport :: BranchSupport -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
cleanLeafSupport maxSup (Node (PhyloLabel l b Nothing) []) = Node (PhyloLabel l (Just maxSup)) []
cleanLeafSupport _ (Node l xs) = Node l $ map cleanLeafSupport xs

toSupportLabel :: PhyloLabel a -> Maybe (Support a)
toSupportLabel (PhyloLabel l _ Nothing) = Nothing
toSupportLabel (PhyloLabel l _ (Just s)) = Support l s
