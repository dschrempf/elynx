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

-}

module EvoMod.Simulate.MarkovProcessAlongTree
  ( -- * Single rate matrix.
    simulateNSitesAlongTree
  , simulateAndFlattenNSitesAlongTree
    -- * Mixture models.
  , simulateNSitesAlongTreeMixtureModel
  , simulateAndFlattenNSitesAlongTreeMixtureModel
  )
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

measureableTreeToProbTree :: (Measurable a) => RateMatrix -> Tree a -> Tree ProbMatrix
measureableTreeToProbTree q = fmap (probMatrix q . measure)

getRootStates :: PrimMonad m
  => Int -> RateMatrix -> Gen (PrimState m) -> m [State]
getRootStates n q g = do
  let d = getStationaryDistribution q
  replicateM n $ categorical d g

-- | Simulate a number of sites for a given substitution model. Only the states
-- at the leafs are retained. The states at internal nodes are removed. This has
-- a lower memory footprint.
simulateAndFlattenNSitesAlongTree :: (PrimMonad m, Measurable a)
  => Int -> RateMatrix -> Tree a -> Gen (PrimState m) -> m [[State]]
simulateAndFlattenNSitesAlongTree n q t g = do
  let pt = measureableTreeToProbTree q t
  is <- getRootStates n q g
  simulateAndFlattenAlongProbTree is pt g

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs. Forget states at internal.
simulateAndFlattenAlongProbTree :: (PrimMonad m)
  => [State] -> Tree ProbMatrix -> Gen (PrimState m) -> m [[State]]
simulateAndFlattenAlongProbTree is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  if null f
    then return [is']
    else concat <$> sequence [simulateAndFlattenAlongProbTree is' t g | t <- f]

-- | Simulate a number of sites for a given substitution model. Keep states at
-- internal nodes. The result is a tree with the list of simulated states as
-- node labels.
simulateNSitesAlongTree :: (PrimMonad m, Measurable a)
  => Int -> RateMatrix -> Tree a -> Gen (PrimState m) -> m (Tree [State])
simulateNSitesAlongTree n q t g = do
  let pt = measureableTreeToProbTree q t
  is <- getRootStates n q g
  simulateAlongProbTree is pt g

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs.
simulateAlongProbTree :: (PrimMonad m)
  => [State] -> Tree ProbMatrix -> Gen (PrimState m) -> m (Tree [State])
simulateAlongProbTree is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  f' <- sequence [simulateAlongProbTree is' t g | t <- f]
  return $ Node is' f'

measureableTreeToProbTreeMixtureModel :: (Measurable a)
  => [RateMatrix] -> Tree a -> Tree [ProbMatrix]
measureableTreeToProbTreeMixtureModel qs =
  fmap (\a -> [probMatrix q . measure $ a | q <- qs])

getComponentsAndRootStates :: PrimMonad m
  => Int -> Vector R -> [RateMatrix] -> Gen (PrimState m) -> m ([Int], [State])
getComponentsAndRootStates n ws qs g = do
  let ds = map getStationaryDistribution qs
  cs <- replicateM n $ categorical ws g
  is <- sequence [ categorical (ds !! c) g | c <- cs ]
  return (cs, is)

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. Forget states at internal nodes. See also
-- 'simulateAndFlattenNSitesAlongTree'.
simulateAndFlattenNSitesAlongTreeMixtureModel :: (PrimMonad m, Measurable a)
  => Int -> Vector R -> [RateMatrix] -> Tree a -> Gen (PrimState m) -> m [[State]]
simulateAndFlattenNSitesAlongTreeMixtureModel n ws qs t g = do
  let pt = measureableTreeToProbTreeMixtureModel qs t
  (cs, is) <- getComponentsAndRootStates n ws qs g
  simulateAndFlattenAlongProbTreeMixtureModel is cs pt g

simulateAndFlattenAlongProbTreeMixtureModel :: (PrimMonad m)
  => [State] -> [Int] -> Tree [ProbMatrix] -> Gen (PrimState m) -> m [[State]]
simulateAndFlattenAlongProbTreeMixtureModel is cs (Node ps f) g
  = do is' <- sequence [ jump i (ps !! c) g | (i, c) <- zip is cs ]
       if null f
         then return [is']
         else concat <$> sequence [ simulateAndFlattenAlongProbTreeMixtureModel is' cs t g | t <- f ]

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. Keep states at internal nodes. See also
-- 'simulateNSitesAlongTree'.
simulateNSitesAlongTreeMixtureModel :: (PrimMonad m, Measurable a)
  => Int -> Vector R -> [RateMatrix] -> Tree a -> Gen (PrimState m) -> m (Tree [State])
simulateNSitesAlongTreeMixtureModel n ws qs t g = do
  let pt = measureableTreeToProbTreeMixtureModel qs t
  (cs, is) <- getComponentsAndRootStates n ws qs g
  simulateAlongProbTreeMixtureModel is cs pt g

-- See 'simulateAlongProbTree', only we have a number of mixture components. The
-- starting states and the components for each site have to be provided.
simulateAlongProbTreeMixtureModel :: (PrimMonad m)
  => [State] -> [Int] -> Tree [ProbMatrix] -> Gen (PrimState m) -> m (Tree [State])
simulateAlongProbTreeMixtureModel is cs (Node ps f) g = do
  is' <- sequence [ jump i (ps !! c) g | (i, c) <- zip is cs ]
  f'  <- sequence [ simulateAlongProbTreeMixtureModel is' cs t g | t <- f ]
  return $ Node is' f'
