{- |
Module      :  EvoMod.Data.Tree.MeasurableTree
Description :  Functions on trees with branch lengths.
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 14:16:34 2019.

-}


module EvoMod.Data.Tree.MeasurableTree
  ( Measurable (..)
  , totalBranchLength
  , height
  ) where

import           Data.Foldable
import           Data.Tree

-- | A 'Node' label with measurable branch length to the parent.
class Measurable a where
  measure :: a -> Double

-- | Total branch length of a tree.
totalBranchLength :: (Measurable a) => Tree a -> Double
totalBranchLength = foldl' (\acc n -> acc + measure n) 0

-- | Distances from the root of the tree to its leafs.
distancesRootLeaves :: (Measurable a) => Tree a -> [Double]
distancesRootLeaves (Node l []) = [measure l]
distancesRootLeaves (Node l f ) = concatMap (map (+ measure l) . distancesRootLeaves) f

-- | Height of a tree.
height :: (Measurable a) => Tree a -> Double
height = maximum . distancesRootLeaves
