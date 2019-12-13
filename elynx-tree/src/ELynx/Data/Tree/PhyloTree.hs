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
import           Data.Tree
import           Test.QuickCheck                   hiding (label)

import           ELynx.Data.Tree.BranchSupportTree
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.NamedTree

-- | A primitive label type for phylogenetic trees with a name, possibly a
-- branch support value, and a 'Double' branch length.
data PhyloLabel a = PhyloLabel { label :: a
                               , brSup :: Maybe Double
                               , brLen :: Double }
                 deriving (Read, Show, Eq)

instance Ord a => Ord (PhyloLabel a) where
  compare = compare `on` label

instance Measurable (PhyloLabel a) where
  getLen = brLen
  setLen l (PhyloLabel lbl s _)
    | l >= 0 = PhyloLabel lbl s l
    | otherwise = error "Branch lengths cannot be negative."

instance BranchSupportLabel (PhyloLabel a) where
  getBranchSupport = brSup
  setBranchSupport Nothing  l = l {brSup = Nothing}
  setBranchSupport (Just s) l
    | s > 0 = l {brSup = Just s}
    | otherwise = error "Branch support cannot be negative."

-- Of course, the boundaries for branch support and length are chosen pretty
-- arbitrarily :).
instance Arbitrary a => Arbitrary (PhyloLabel a) where
  arbitrary = PhyloLabel
    <$> arbitrary
    <*> (Just <$> choose (0, 100))
    <*> choose (0, 10)

instance Named a => Named (PhyloLabel a) where
  getName = getName . label

-- | Remove branch lengths from tree.
removeBrInfo :: Tree (PhyloLabel a) -> Tree a
removeBrInfo = fmap label
