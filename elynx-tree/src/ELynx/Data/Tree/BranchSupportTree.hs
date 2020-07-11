-- |
-- Module      :  ELynx.Data.Tree.BranchSupportTree
-- Description :  Node label with branch support
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jun 13 14:06:45 2019.
module ELynx.Data.Tree.BranchSupportTree
  ( BranchSupport,
    BranchSupported (..),
    normalizeBranchSupport,
    collapse,
  )
where

import Data.List
import Data.Tree

-- | Branch support.
type BranchSupport = Double

-- | A label that supports extraction and setting of branch support values.
class BranchSupported a where
  getBranchSupport :: a -> BranchSupport
  setBranchSupport :: BranchSupport -> a -> a

apply :: BranchSupported a => (Double -> Double) -> a -> a
apply f l = setBranchSupport (f s) l where s = getBranchSupport l

-- | Normalize branch support values. The maximum branch support value will be
-- set to 1.0.
normalizeBranchSupport :: BranchSupported a => Tree a -> Tree a
normalizeBranchSupport t = fmap (apply (/ m)) t
  where m = maximum $ fmap getBranchSupport t

accept :: Double -> Double -> Bool
accept thresh s = s >= thresh

-- TODO: Something is wrong here. @collapse 1.0 t@ should be a star tree but it
-- is a leaf.
-- | Collapse branches with support lower than given value. Note, branch length
-- of collapsed branches is ignored at the moment. Continue collapsing until a
-- fix point is reached.
collapse :: (Show a, Eq a, BranchSupported a) => BranchSupport -> Tree a -> Tree a
collapse th tr = if tr == tr' then tr else collapse th tr'
  where
    tr' = collapse' th tr

-- | See 'collapse'.
collapse' :: BranchSupported a => BranchSupport -> Tree a -> Tree a
collapse' _ t@(Node _ []) = t
collapse' th (Node l xs) = Node l $ map (collapse' th) (highS ++ lowSubForest)
  where
    (highS, lowS) = partition (accept th . getBranchSupport . rootLabel) xs
    lowSubForest = concatMap subForest lowS
