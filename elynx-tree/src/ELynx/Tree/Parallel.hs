-- |
-- Module      :  ELynx.Tree.Parallel
-- Description :  Evaluation strategies for trees
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Sep  7 13:36:45 2020.
--
-- Parallel evaluation up to a given layer. By convention layer 0 only has one
-- element: the root node. The layer 1 includes the daughter nodes of the root
-- node, and so on.
--
-- The layer to which a node belongs should not be confused with the 'depth' of
-- a tree.
module ELynx.Tree.Parallel
  ( parTree,
    parFoldMapTree',
  )
where

import Control.Parallel.Strategies
import Data.Foldable
import ELynx.Tree.Rooted

myParList :: Strategy a -> Strategy [a]
myParList _ [] = return []
myParList s xs = do
  ys <- parList s $ tail xs
  y <- s $ head xs
  return $ y : ys

-- | Parallel evaluation strategy for a tree into normal form.
--
-- Evaluate the sub trees up to given layer in parallel.
parTree :: NFData a => Int -> Strategy (Tree a)
parTree n t@(Node lb ts)
  | n == 1 = do
    ts' <- myParList rdeepseq ts
    return $ Node lb ts'
  | n >= 2 = do
    ts' <- myParList (parTree (n -1)) ts
    return $ Node lb ts'
  | otherwise = rdeepseq t

-- | Strict 'foldMap'' of a tree with parallel evaluation of the sub trees up to
-- a given layer.
parFoldMapTree' :: (NFData m, Monoid m) => Int -> (a -> m) -> Tree a -> m
parFoldMapTree' n f t@(Node lb ts)
  | n >= 1 = foldl' mappend (f lb) (map (parFoldMapTree' (n - 1) f) ts `using` myParList rdeepseq)
  | otherwise = foldMap' f t
