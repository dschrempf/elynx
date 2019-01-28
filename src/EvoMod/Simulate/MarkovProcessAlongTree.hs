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

module EvoMod.Simulate.MarkovProcessAlongTree
  (simulateNSitesAlongTree)
  where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Tree
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           EvoMod.Data.RateMatrix.RateMatrix
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Simulate.MarkovProcess

-- | Simulate a number of site for a given substitution model with given
-- stationary distribution. The result is a tree with the list of simulated
-- states as node labels.
simulateNSitesAlongTree :: (PrimMonad m, Measurable a) => Int -> RateMatrix -> Tree a -> Gen (PrimState m) -> m (Tree [State])
simulateNSitesAlongTree n q t g = do
  let d = getStationaryDistribution q
  is <- replicateM n $ categorical d g
  simulateAlongProbTree is pt g
  where pt = measureableTreeToProbTree q t

measureableTreeToProbTree :: (Measurable a) => RateMatrix -> Tree a -> Tree ProbMatrix
measureableTreeToProbTree q = fmap (probMatrix q . measure)

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs.
simulateAlongProbTree :: (PrimMonad m) => [State] -> Tree ProbMatrix -> Gen (PrimState m) -> m (Tree [State])
simulateAlongProbTree is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  f' <- sequence [simulateAlongProbTree is' t g | t <- f]
  return $ Node is' f'
-- simulateAlongProbTree :: (PrimMonad m) => State -> Tree ProbMatrix -> Gen (PrimState m) -> m (Tree State)
-- simulateAlongProbTree i (Node p f) g = return $ Node i []

-- -- | Convert a stationary distribution to a generator which speeds up random
-- -- picks. Cf. 'jump'.
-- stationaryDistToGenerator :: StationaryDist -> Generator State
-- stationaryDistToGenerator f = fG
--   -- This is a little complicated. I need to convert the vector to a list to be
--   -- able to create a distribution.
--   where !fL = toList f
--         !fD = fromList $ zip (map State [0..]) fL
--         !fG = fromDistribution fD

-- -- | See 'jump'.
-- treeProbMatrixToTreeGenerator :: RTree a ProbMatrix -> RTree a [Generator State]
-- treeProbMatrixToTreeGenerator t = tG
--   where
--     -- Create a tree with the probability matrices as list of row vectors.
--     !tL = fmap toLists t
--     -- A complicated double map. We need to create generators for each branch on
--     -- the tree (fmap) and for each target state on each branch (map).
--     !tG = (fmap . map) (fromDistribution . fromList . zip (map State [0..])) tL

-- -- | Simulate data (states at the leaves) for a tree with transition
-- -- probabilities on its branches and with the stationary distribution of states
-- -- at the root. The state at the root is randomly chosen from the stationary
-- -- distribution and the states at the nodes and leaves are randomly chosen
-- -- according to the transition probabilities.
-- simulateSite :: (MonadRandom m) =>
--                 Generator State
--              -> RTree a [Generator State]
--              -> m [(a, State)]
-- simulateSite f t = do
--   !rootState <- getSample f
--   populateAndFlattenTree t rootState

-- -- | Randomly draw an index according to a given generator. Use the stationary
-- -- distribution and rooted tree at the drawn index to simulate a site. This is
-- -- useful for simulation, e.g., Gamma rate heterogeneity models.
-- simulateSiteGen :: (MonadRandom m) =>
--                    Generator Int
--                 -> [Generator State]
--                 -> [RTree a [Generator State]]
--                 -> m [(a, State)]
-- simulateSiteGen gen fs trs = do
--   !i <- getSample gen
--   let !f = fs  !! i
--       !t = trs !! i
--   simulateSite f t
