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
  ( MeasurableNode (..)
  , totalBranchLength
  , height
  ) where

import           Data.Foldable
import           Data.Tree

-- | A tree node with measurable branch length.
class MeasurableNode n where
  branchLength :: n -> Double

-- | Total branch length of a tree.
totalBranchLength :: (MeasurableNode n) => Tree n -> Double
totalBranchLength = foldl' (\acc n -> acc + branchLength n) 0

-- | Distances from the root of the tree to its leafs.
distancesRootLeaves :: (MeasurableNode n) => Tree n -> [Double]
distancesRootLeaves (Node s []) = [branchLength s]
distancesRootLeaves (Node s f ) = concatMap (map (+ branchLength s) . distancesRootLeaves) f

-- | Height of a tree.
height :: (MeasurableNode n) => Tree n -> Double
height = maximum . distancesRootLeaves
