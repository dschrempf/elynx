{- |
   Description :  Functions to work with transition probability matrices on rooted trees
   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Calculate transition probability matrices, map rate matrices on trees, populate
a tree with states according to a stationary distribution, etc.

The implementation of the Markov process is more than basic and can be improved in a lot of ways.

* Changelog

-}

module EvoMod.Simulate.MultiSequenceAlignment
  (simulateMSA)
  where

import           Control.Monad.Primitive
import           Data.Tree

import           EvoMod.Data.RateMatrix.RateMatrix
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Tree.MeasurableTree
-- import           EvoMod.Simulate.MarkovChainAlongTree

-- TODO: Need IDs. Maybe create something link 'IdentifiableLabel' or 'HasID'.

-- On the other hand, I think that these cross module dependencies are very
-- bad. Maybe just leave the solution below.

-- MSA _ n

-- | Simulate a 'MultiSequenceAlignment' for a given substitution model with
-- given stationary distribution and a phylogenetic tree.
simulateMSA :: (PrimMonad m, MeasurableLabel a) => Int -> RateMatrix -> StationaryDist -> Tree a -> m MultiSequenceAlignment
-- simulateMSA n q d t g = simulateNSitesAlongTree n q d t g
simulateMSA = undefined
