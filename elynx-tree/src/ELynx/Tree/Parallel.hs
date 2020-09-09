-- |
-- Module      :  ELynx.Tree.Parallel
-- Description :  Evaluation strategies for trees
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Sep  7 13:36:45 2020.
module ELynx.Tree.Parallel
  ( parTree,
    parBranchFoldMap,
    parNodeFoldMap,
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
  return $ y:ys

-- | Parallel evaluation strategy for a tree into normal form.
--
-- Evaluate the sub trees up to given layer in parallel.
parTree :: (NFData e, NFData a) => Int -> Strategy (Tree e a)
parTree n t@(Node br lb ts)
  | n == 0 = rdeepseq t
  | n == 1 = do
      ts' <- myParList rdeepseq ts
      return $ Node br lb ts'
  | n >= 2 = do
      ts' <- myParList (parTree (n-1)) ts
      return $ Node br lb ts'
  | otherwise = error "parTree: n is negative."

branchFoldMap :: (e -> f) -> (f -> f -> f) -> Tree e a -> f
branchFoldMap f op (Node br _ ts) = foldl' op (f br) $ map (branchFoldMap f op) ts

-- | Map and fold over branches. Evaluate the sub trees up to given layer in parallel.
parBranchFoldMap :: NFData f => Int -> (e -> f) -> (f -> f -> f) -> Tree e a -> f
parBranchFoldMap 0 f op t = branchFoldMap f op t
parBranchFoldMap n f op (Node br _ ts)
  | n >= 1 = foldl' op (f br) (map (parBranchFoldMap (n - 1) f op) ts `using` myParList rdeepseq)
  | otherwise = error "parBranchFoldMap: n is negative."

nodeFoldMap :: (a -> b) -> (b -> b -> b) -> Tree e a -> b
nodeFoldMap f op (Node _ lb ts) = foldl' op (f lb) $ map (nodeFoldMap f op) ts

-- | Map and fold over nodes. Evaluate the sub trees up to given layer in parallel.
parNodeFoldMap :: NFData b => Int -> (a -> b) -> (b -> b -> b) -> Tree e a -> b
parNodeFoldMap 0 f op t = nodeFoldMap f op t
parNodeFoldMap n f op (Node _ lb ts)
  | n >= 1 = foldl' op (f lb) (map (parNodeFoldMap (n - 1) f op) ts `using` myParList rdeepseq)
  | otherwise = error "parNodeFoldMap: n is negative."
