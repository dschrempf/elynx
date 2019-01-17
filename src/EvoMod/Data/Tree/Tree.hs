{- |
Module      :  EvoMod.Data.Tree.Tree
Description :  Functions related to phylogenetic trees.
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 09:57:29 2019.

What is called /label/ in 'Data.Tree', I call /node/.

Branches have /length/s, not distances.

-}


module EvoMod.Data.Tree.Tree
  (
  ) where

import Data.Tree
import Data.Foldable

class MeasureableNode n where
  branchLength :: (Num b) => n -> b

class PhyloNode n where
  internal        :: n -> Bool
  extant          :: n -> Bool
  extinct         :: n -> Bool
  external        :: n -> Bool

  internal n = not $ extant n || extinct n
  external   = not . internal

totalBrLn :: (Num b, MeasureableNode n) => Tree n -> b
totalBrLn = foldl' (\acc n -> acc + branchLength n) 0

distancesRootLeaves :: (Num b, MeasureableNode n) => Tree n -> [b]
distancesRootLeaves (Node s []) = [branchLength s]
distancesRootLeaves (Node s f ) = concatMap (map (+ branchLength s) . distancesRootLeaves) f

height :: (Num b, Ord b, MeasureableNode n) => Tree n -> b
height = maximum . distancesRootLeaves
