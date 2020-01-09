{-# LANGUAGE FlexibleInstances #-}

{- |
Module      :  ELynx.Data.Tree.PhyloTree
Description :  Phylogenetic trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 16:08:54 2019.

Phylogenetic nodes labels, aka 'PhyloLabel's, have a branch length and an
arbitrary label type, e.g., of type 'Int'.

-}


module ELynx.Data.Tree.PhyloTree
  ( PhyloLabel (..)
  , removeBrInfo
  ) where

import           Data.Function
import           Data.Maybe                        (fromMaybe)
import           Data.Tree
import           Test.QuickCheck                   hiding (label)

import           ELynx.Data.Tree.BranchSupportTree
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.NamedTree

-- | A primitive label type for phylogenetic trees with a name, possibly a
-- branch support value, and possibly a branch length.
data PhyloLabel a = PhyloLabel { label :: a
                               , brSup :: Maybe Double
                               , brLen :: Maybe Double }
                  deriving (Read, Show, Eq)

instance Ord a => Ord (PhyloLabel a) where
  compare = compare `on` label

-- | If no branch length is available, 0 is returned. This is probably not the
-- best (and final) behavior.
instance Measurable (PhyloLabel a) where
  getLen = fromMaybe 0 . brLen
  setLen l x
    | l >= 0 = x {brLen = Just l}
    | otherwise = error "Branch lengths cannot be negative."

instance BranchSupported (PhyloLabel a) where
  getBranchSupport = brSup
  setBranchSupport Nothing  l = l {brSup = Nothing}
  setBranchSupport (Just s) l
    | s > 0 = l {brSup = Just s}
    | otherwise = error "Branch support cannot be negative."

-- Of course, the boundaries for branch support and length are chosen pretty
-- arbitrarily :).
--
-- XXX: This instance does not produce values without branch lengths nor branch
-- supports.
instance Arbitrary a => Arbitrary (PhyloLabel a) where
  arbitrary = PhyloLabel
    <$> arbitrary
    <*> (Just <$> choose (0, 100))
    <*> (Just <$> choose (0, 10) )

instance Named a => Named (PhyloLabel a) where
  getName = getName . label

-- | Remove all branch relevant information from all nodes of the tree; only
-- retain the labels.
removeBrInfo :: Tree (PhyloLabel a) -> Tree a
removeBrInfo = fmap label
