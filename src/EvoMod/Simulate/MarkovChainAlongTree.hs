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

module EvoMod.Simulate.MarkovChainAlongTree
  (simulateNSitesAlongTree)
  where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Tree
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           EvoMod.Data.RateMatrix.RateMatrix
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Simulate.MarkovChain

-- -- | Convert a tree with branch lengths into a tree that has transition
-- -- probability matrices assigned to each of its branches.
-- branchLengthsToTransitionProbs :: RateMatrix -> RTree a Double -> RTree a ProbMatrix
-- branchLengthsToTransitionProbs m = fmap (probMatrix m)

-- | Simulate a number of site for a given substitution model with given
-- stationary distribution.
--
-- TODO: Extract stationary distribution.
--
-- The result is a set of trees with the simulated states as node labels.
simulateNSitesAlongTree :: (PrimMonad m, MeasurableLabel a) => Int -> RateMatrix -> StationaryDist -> Tree a -> Gen (PrimState m) -> m [Tree State]
simulateNSitesAlongTree n q d t g = do
  i <- categorical d g
  replicateM n $ simulateAlongProbTree i pt g
  where pt = measureableTreeToProbTree q t

measureableTreeToProbTree :: (MeasurableLabel a) => RateMatrix -> Tree a -> Tree ProbMatrix
measureableTreeToProbTree q = fmap (probMatrix q . branchLength)

-- This is the heart of the simulation. Take a tree and a root state. If there
-- is a split (i.e., if we have a node and not a leaf), jump down the left
-- branch and populate and flatten the tree and jump down the right branch and
-- populate and flatten the tree and append the two results. This is what
-- 'liftM2' is doing, it lifts the append function of lists (++) to the 'Rand g'
-- monad. If we encounter a leaf, just return the node label (or whatever the
-- type a is) and the state that we ended up at.
simulateAlongProbTree :: (PrimMonad m) => State -> Tree ProbMatrix -> Gen (PrimState m) -> m (Tree State)
simulateAlongProbTree i (Node p f) g = do
  i' <- jump i p g
  f' <- sequence [simulateAlongProbTree i' t g | t <- f]
  return $ Node i' f'

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
