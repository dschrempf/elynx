{- |
Module      :  ELynx.Data.Tree.BranchSupportTree
Description :  Node label with branch support
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jun 13 14:06:45 2019.

-}

module ELynx.Data.Tree.BranchSupportTree
  ( BranchSupport
  , BranchSupported (..)
  , normalize
  , collapse
  ) where

import           Data.List
import           Data.Tree

-- TODO: Implement the preferred way:
-- data BranchSupport =
--   BSNothing
--   | BSInt Int
--   | BSDouble Double
--   deriving (Num)

-- | At the moment, just use 'Double'. It would be preferable to use a wrapper
-- data type that can handle 'Int' or 'Double'.
type BranchSupport = Maybe Double

-- | A label that supports extraction and setting of branch support values.
class BranchSupported a where
  -- | For now, branch support is a Double, but one could also think about
  -- bootstrap values, which are integers.
  getBranchSupport :: a -> BranchSupport
  setBranchSupport :: BranchSupport -> a -> a

apply :: BranchSupported a => (Double -> Double) -> a -> a
apply f l = setBranchSupport (f <$> s) l
  where s = getBranchSupport l

-- | Normalize branch support values. The maximum branch support value will be
-- set to 1.0.
normalize :: BranchSupported a => Tree a -> Tree a
normalize t = case mm of
  Nothing -> t
  Just m  -> fmap (apply (/ m)) t
  where mm = maximum $ fmap getBranchSupport t

accept :: Double -> Maybe Double -> Bool
accept _       Nothing = True
accept thresh (Just s) = s > thresh

-- | Collapse branches with support lower than given value. Note, branch length
-- is ignored at the moment. Continue collapsing until a fix point is reached.
collapse :: (Show a, Eq a, BranchSupported a) => Double -> Tree a -> Tree a
collapse th tr = if tr == tr'
                 then tr
                 else collapse th tr'
  where tr' = collapse' th tr

-- | See 'collapse'.
collapse' :: BranchSupported a => Double -> Tree a -> Tree a
collapse' _  t@(Node _ []) = t
collapse' th   (Node l xs) = Node l $ map (collapse' th) (highS ++ lowSubForest)
  where (highS, lowS) = partition (accept th . getBranchSupport . rootLabel) xs
        lowSubForest = concatMap subForest lowS
