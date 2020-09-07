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
    parBranchFold,
    parBranchFoldMap,
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

branchFold :: (e -> e -> e) -> Tree e a -> e
branchFold f (Node br _ ts) = foldl' f br $ map (branchFold f) ts

-- | Fold over branches. Evaluate the given layer in parallel.
parBranchFold :: NFData e => Int -> (e -> e -> e) -> Tree e a -> e
parBranchFold 0 f t = branchFold f t
parBranchFold 1 f (Node br _ ts) =
  foldl' f br (map (branchFold f) ts `using` parList rdeepseq)
parBranchFold n f (Node br _ ts)
  | n >= 2 = foldl' f br $ map (parBranchFold (n - 1) f) ts
  | otherwise = error "parBranchFold: n is negative."

branchFoldMap :: (e -> f) -> (f -> f -> f) -> Tree e a -> f
branchFoldMap f g (Node br _ ts) = foldl' g (f br) $ map (branchFoldMap f g) ts

-- | Map and fold over branches. Evaluate the given layer in parallel.
parBranchFoldMap :: NFData f => Int -> (e -> f) -> (f -> f -> f) -> Tree e a -> f
parBranchFoldMap 0 f g t = branchFoldMap f g t
parBranchFoldMap 1 f g (Node br _ ts) =
  foldl' g (f br) (map (branchFoldMap f g) ts `using` parList rdeepseq)
parBranchFoldMap n f g (Node br _ ts)
  | n >= 2 = foldl' g (f br) $ map (parBranchFoldMap (n - 1) f g) ts
  | otherwise = error "parBranchFoldMap: n is negative."
