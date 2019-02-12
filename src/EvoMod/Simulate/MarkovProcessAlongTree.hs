{- |
   Description :  Functions to work with transition probability matrices on rooted trees
   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Calculate transition probability matrices, map rate matrices on trees, populate
a tree with states according to a stationary distribution, etc.

The implementation of the Markov process is more than basic and can be improved
in a lot of ways.

* Changelog

-}

module EvoMod.Simulate.MarkovProcessAlongTree
  ( simulateNSitesAlongTree
  , simulateNSitesAlongTreeMixtureModel )
  where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Tree
import           Numeric.LinearAlgebra
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Simulate.MarkovProcess

-- | Simulate a number of sites for a given substitution model with given
-- stationary distribution. The result is a tree with the list of simulated
-- states as node labels.
simulateNSitesAlongTree :: (PrimMonad m, Measurable a) => Int -> RateMatrix -> Tree a -> Gen (PrimState m) -> m (Tree [State])
simulateNSitesAlongTree n q t g = do
  let d  = getStationaryDistribution q
      pt = measureableTreeToProbTree q t
  is <- replicateM n $ categorical d g
  simulateAlongProbTree is pt g

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. See also 'simulateNSitesAlongTree'.
simulateNSitesAlongTreeMixtureModel :: (PrimMonad m, Measurable a) => Int -> Vector R -> [RateMatrix] -> Tree a -> Gen (PrimState m) -> m (Tree [State])
simulateNSitesAlongTreeMixtureModel n ws qs t g = do
  let ds = map getStationaryDistribution qs
      pt = measureableTreeToProbTreeMixtureModel qs t
  cs <- replicateM n $ categorical ws g
  is <- sequence [ categorical (ds !! c) g | c <- cs ]
  simulateAlongProbTreeMixtureModel is cs pt g

measureableTreeToProbTreeMixtureModel :: (Measurable a) => [RateMatrix] -> Tree a -> Tree [ProbMatrix]
measureableTreeToProbTreeMixtureModel qs = fmap (\a -> [probMatrix q . measure $ a | q <- qs])

-- See 'simulateAlongProbTree', only we have a number of mixture components. The
-- starting states and the components for each site have to be provided.

-- XXX: The whole tree is stored in memory. Solution:
-- populateAndFlattenTree :: (MonadRandom m) => RTree a [Generator State] -> State -> m [(a, State)]
-- populateAndFlattenTree (Leaf a) s = return [(a, s)]
-- populateAndFlattenTree (Node _ lp lc rp rc) s = liftM2 (++) (jumpDownBranch lp lc) (jumpDownBranch rp rc)
--   where jumpDownBranch p t = jump s p >>= populateAndFlattenTree 
simulateAlongProbTreeMixtureModel :: (PrimMonad m) => [State] -> [Int] -> Tree [ProbMatrix] -> Gen (PrimState m) -> m (Tree [State])
simulateAlongProbTreeMixtureModel is cs (Node ps f) g = do
  is' <- sequence [ jump i (ps !! c) g | (i, c) <- zip is cs ]
  f'  <- sequence [ simulateAlongProbTreeMixtureModel is' cs t g | t <- f ]
  return $ Node is' f'

measureableTreeToProbTree :: (Measurable a) => RateMatrix -> Tree a -> Tree ProbMatrix
measureableTreeToProbTree q = fmap (probMatrix q . measure)

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs.
simulateAlongProbTree :: (PrimMonad m) => [State] -> Tree ProbMatrix -> Gen (PrimState m) -> m (Tree [State])
simulateAlongProbTree is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  f' <- sequence [simulateAlongProbTree is' t g | t <- f]
  return $ Node is' f'
