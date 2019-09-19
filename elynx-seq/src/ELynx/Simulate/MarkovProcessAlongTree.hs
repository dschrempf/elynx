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

module ELynx.Simulate.MarkovProcessAlongTree
  ( -- * Single rate matrix.
    simulateNSitesAlongTree
  , simulateAndFlattenNSitesAlongTree
    -- * Mixture models.
  , simulateNSitesAlongTreeMixtureModel
  , simulateAndFlattenNSitesAlongTreeMixtureModel
  , simulateAndFlattenNSitesAlongTreeMixtureModelPar
  )
  where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Parallel.Strategies
import           Data.Tree
import           Numeric.LinearAlgebra
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           ELynx.Data.MarkovProcess.RateMatrix
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Simulate.MarkovProcess
import           ELynx.Tools.Concurrent

measurableTreeToProbTree :: (Measurable a) => RateMatrix -> Tree a -> Tree ProbMatrix
measurableTreeToProbTree q = fmap (probMatrix q . getLen)

getRootStates :: PrimMonad m
  => Int -> StationaryDistribution -> Gen (PrimState m) -> m [State]
getRootStates n d g = replicateM n $ categorical d g

-- | Simulate a number of sites for a given substitution model. Only the states
-- at the leafs are retained. The states at internal nodes are removed. This has
-- a lower memory footprint.
simulateAndFlattenNSitesAlongTree :: (PrimMonad m, Measurable a)
  => Int -> StationaryDistribution -> ExchangeabilityMatrix -> Tree a -> Gen (PrimState m) -> m [[State]]
simulateAndFlattenNSitesAlongTree n d e t g = do
  let q = fromExchangeabilityMatrix e d
      pt = measurableTreeToProbTree q t
  is <- getRootStates n d g
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
  => Int -> StationaryDistribution -> ExchangeabilityMatrix -> Tree a -> Gen (PrimState m) -> m (Tree [State])
simulateNSitesAlongTree n d e t g = do
  let q = fromExchangeabilityMatrix e d
      pt = measurableTreeToProbTree q t
  is <- getRootStates n d g
  simulateAlongProbTree is pt g

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs.
simulateAlongProbTree :: (PrimMonad m)
  => [State] -> Tree ProbMatrix -> Gen (PrimState m) -> m (Tree [State])
simulateAlongProbTree is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  f' <- sequence [simulateAlongProbTree is' t g | t <- f]
  return $ Node is' f'

measurableTreeToProbTreeMixtureModel :: (Measurable a)
  => [RateMatrix] -> Tree a -> Tree [ProbMatrix]
measurableTreeToProbTreeMixtureModel qs =
  -- TODO: This doesn't work at all! Repair this.
  fmap (\a -> [probMatrix q . getLen $ a | q <- qs] `using` parList rpar)
  -- fmap (\a -> [probMatrix q . getLen $ a | q <- qs])

getComponentsAndRootStates :: PrimMonad m
  => Int -> Vector R -> [StationaryDistribution] -> Gen (PrimState m) -> m ([Int], [State])
getComponentsAndRootStates n ws ds g = do
  cs <- replicateM n $ categorical ws g
  is <- sequence [ categorical (ds !! c) g | c <- cs ]
  return (cs, is)

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. Forget states at internal nodes. See also
-- 'simulateAndFlattenNSitesAlongTree'.
simulateAndFlattenNSitesAlongTreeMixtureModel :: (PrimMonad m, Measurable a)
  => Int -> Vector R -> [StationaryDistribution] -> [ExchangeabilityMatrix] -> Tree a
  -> Gen (PrimState m) -> m [[State]]
simulateAndFlattenNSitesAlongTreeMixtureModel n ws ds es t g = do
  let qs = zipWith fromExchangeabilityMatrix es ds
      pt = measurableTreeToProbTreeMixtureModel qs t
  (cs, is) <- getComponentsAndRootStates n ws ds g
  simulateAndFlattenAlongProbTreeMixtureModel is cs pt g

simulateAndFlattenAlongProbTreeMixtureModel :: (PrimMonad m)
  => [State] -> [Int] -> Tree [ProbMatrix] -> Gen (PrimState m) -> m [[State]]
simulateAndFlattenAlongProbTreeMixtureModel is cs (Node ps f) g
  = do is' <- sequence [ jump i (ps !! c) g | (i, c) <- zip is cs ]
       if null f
         then return [is']
         else concat <$> sequence [ simulateAndFlattenAlongProbTreeMixtureModel is' cs t g | t <- f ]

-- | See 'simulateAndFlattenNSitesAlongTreeMixtureModel', parallel version;
-- needs to be run in IO monad.
simulateAndFlattenNSitesAlongTreeMixtureModelPar
  :: Measurable a
  => Int -> Vector R -> [StationaryDistribution] -> [ExchangeabilityMatrix] -> Tree a
  -> GenIO -> IO [[[State]]]
simulateAndFlattenNSitesAlongTreeMixtureModelPar n ws ds es t g = do
  let qs = zipWith fromExchangeabilityMatrix es ds
      pt = measurableTreeToProbTreeMixtureModel qs t
  parComp n (\n' g' -> do
                (cs, is) <- getComponentsAndRootStates n' ws ds g'
                simulateAndFlattenAlongProbTreeMixtureModel is cs pt g') g

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. Keep states at internal nodes. See also
-- 'simulateNSitesAlongTree'.
simulateNSitesAlongTreeMixtureModel :: (PrimMonad m, Measurable a)
  => Int -> Vector R -> [StationaryDistribution] -> [ExchangeabilityMatrix] -> Tree a
  -> Gen (PrimState m) -> m (Tree [State])
simulateNSitesAlongTreeMixtureModel n ws ds es t g = do
  let qs = zipWith fromExchangeabilityMatrix es ds
      pt = measurableTreeToProbTreeMixtureModel qs t
  (cs, is) <- getComponentsAndRootStates n ws ds g
  simulateAlongProbTreeMixtureModel is cs pt g

-- See 'simulateAlongProbTree', only we have a number of mixture components. The
-- starting states and the components for each site have to be provided.
simulateAlongProbTreeMixtureModel :: (PrimMonad m)
  => [State] -> [Int] -> Tree [ProbMatrix] -> Gen (PrimState m) -> m (Tree [State])
simulateAlongProbTreeMixtureModel is cs (Node ps f) g = do
  is' <- sequence [ jump i (ps !! c) g | (i, c) <- zip is cs ]
  f'  <- sequence [ simulateAlongProbTreeMixtureModel is' cs t g | t <- f ]
  return $ Node is' f'
