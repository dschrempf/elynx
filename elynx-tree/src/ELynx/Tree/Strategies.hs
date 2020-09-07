-- |
-- Module      :  ELynx.Tree.Strategies
-- Description :  Evaluation strategies for trees
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Sep  7 13:36:45 2020.
module ELynx.Tree.Strategies
  ( parTree,
    using,
    parBranchFoldMap,
    parNodeFoldMap,
  )
where

import Control.Parallel.Strategies
import Data.Foldable
import ELynx.Tree.Rooted

-- | Parallel evaluation strategy for a tree.
--
-- Evaluate the sub trees at given layer in parallel.
parTree :: (NFData e, NFData a) => Int -> Strategy (Tree e a)
parTree 0 t = rdeepseq t
parTree n (Node br lb ts)
  | n == 1 = evalTreeWith parList
  | n >= 2 = evalTreeWith evalList
  | otherwise = error "parTree: n is negative."
  where
    evalTreeWith strat = do
      ts' <- strat (parTree $ n - 1) ts
      br' <- rdeepseq br
      lb' <- rdeepseq lb
      return $ Node br' lb' ts'

branchFoldMap :: (e -> f) -> (f -> f -> f) -> Tree e a -> f
branchFoldMap f op (Node br _ ts) = foldl' op (f br) $ map (branchFoldMap f op) ts

-- | Map and fold over branches. Evaluate the sub trees at given layer in parallel.
parBranchFoldMap :: NFData f => Int -> (e -> f) -> (f -> f -> f) -> Tree e a -> f
parBranchFoldMap 0 f op t = branchFoldMap f op t
parBranchFoldMap 1 f op (Node br _ ts) =
  foldl' op (f br) (map (branchFoldMap f op) ts `using` parList rdeepseq)
parBranchFoldMap n f op (Node br _ ts)
  | n >= 2 = foldl' op (f br) $ map (parBranchFoldMap (n - 1) f op) ts
  | otherwise = error "parBranchFoldMap: n is negative."

nodeFoldMap :: (a -> b) -> (b -> b -> b) -> Tree e a -> b
nodeFoldMap f op (Node _ lb ts) = foldl' op (f lb) $ map (nodeFoldMap f op) ts

-- | Map and fold over nodes. Evaluate the sub trees at given layer in parallel.
parNodeFoldMap :: NFData b => Int -> (a -> b) -> (b -> b -> b) -> Tree e a -> b
parNodeFoldMap 0 f op t = nodeFoldMap f op t
parNodeFoldMap 1 f op (Node _ lb ts) =
  foldl' op (f lb) (map (nodeFoldMap f op) ts `using` parList rdeepseq)
parNodeFoldMap n f op (Node _ lb ts)
  | n >= 2 = foldl' op (f lb) $ map (parNodeFoldMap (n - 1) f op) ts
  | otherwise = error "parNodeFoldMap: n is negative."
