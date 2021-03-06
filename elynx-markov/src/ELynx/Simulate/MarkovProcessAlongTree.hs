{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   Description :  Work with transition probability matrices on rooted trees
--   Copyright   :  (c) Dominik Schrempf 2021
--   License     :  GPLv3
--
--   Maintainer  :  dominik.schrempf@gmail.com
--   Stability   :  unstable
--   Portability :  non-portable (not tested)
--
-- Calculate transition probability matrices, map rate matrices on trees, populate
-- a tree with states according to a stationary distribution, etc.
--
-- The implementation of the Markov process is more than basic and can be improved
-- in a lot of ways.
module ELynx.Simulate.MarkovProcessAlongTree
  ( -- * Single rate matrix.
    simulate,
    simulateAndFlatten,
    simulateAndFlattenPar,

    -- * Mixture models.
    simulateMixtureModel,
    simulateAndFlattenMixtureModel,
    simulateAndFlattenMixtureModelPar,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Primitive
import Data.Tree
import qualified Data.Vector as V
import Data.Word (Word32)
import ELynx.Data.MarkovProcess.RateMatrix
import ELynx.Simulate.MarkovProcess
import System.Random.MWC
import System.Random.MWC.Distributions (categorical)

-- XXX @performace. The horizontal concatenation might be slow. If so,
-- 'concatenateSeqs' or 'concatenateAlignments' can be used, which directly
-- appends vectors.

-- A brain f***. As an example, let @xss@ be a list of alignments (i.e., a list
-- of a list of a list of alleles). This function horizontally concatenates the
-- sites. The number of species needs to be same in each alignment. No checks
-- are performed!
horizontalConcat :: [[[a]]] -> [[a]]
horizontalConcat [xs] = xs
horizontalConcat xss = foldr1 (zipWith (++)) xss

toProbTree :: RateMatrix -> Tree Double -> Tree ProbMatrix
toProbTree q = fmap (probMatrix q)

getRootStates ::
  PrimMonad m =>
  Int ->
  StationaryDistribution ->
  Gen (PrimState m) ->
  m [State]
getRootStates n d g = replicateM n $ categorical d g

-- | Simulate a number of sites for a given substitution model. Only the states
-- at the leafs are retained. The states at internal nodes are removed. This has
-- a lower memory footprint.

-- XXX: Improve performance. Use vectors, not lists. I am actually not sure if
-- this improves performance...
simulateAndFlatten ::
  PrimMonad m =>
  Int ->
  StationaryDistribution ->
  ExchangeabilityMatrix ->
  Tree Double ->
  Gen (PrimState m) ->
  m [[State]]
simulateAndFlatten n d e t g = do
  let q = fromExchangeabilityMatrix e d
      pt = toProbTree q t
  is <- getRootStates n d g
  simulateAndFlatten' is pt g

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs. Forget states at internal
-- nodes.
simulateAndFlatten' ::
  (PrimMonad m) =>
  [State] ->
  Tree ProbMatrix ->
  Gen (PrimState m) ->
  m [[State]]
simulateAndFlatten' is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  if null f
    then return [is']
    else concat <$> sequence [simulateAndFlatten' is' t g | t <- f]

-- | See 'simulateAndFlatten', parallel version.
simulateAndFlattenPar ::
  Int ->
  StationaryDistribution ->
  ExchangeabilityMatrix ->
  Tree Double ->
  GenIO ->
  IO [[State]]
simulateAndFlattenPar n d e t g = do
  c <- getNumCapabilities
  gs <- splitGen c g
  let chunks = getChunks c n
      q = fromExchangeabilityMatrix e d
      pt = toProbTree q t
  -- The concurrent map returns a list of [[State]] objects. They have to be
  -- concatenated horizontally.
  sss <-
    mapConcurrently
      ( \(num, gen) -> do
          is <- getRootStates num d gen
          simulateAndFlatten' is pt gen
      )
      (zip chunks gs)
  return $ horizontalConcat sss

-- | Simulate a number of sites for a given substitution model. Keep states at
-- internal nodes. The result is a tree with the list of simulated states as
-- node labels.
simulate ::
  PrimMonad m =>
  Int ->
  StationaryDistribution ->
  ExchangeabilityMatrix ->
  Tree Double ->
  Gen (PrimState m) ->
  m (Tree [State])
simulate n d e t g = do
  let q = fromExchangeabilityMatrix e d
      pt = toProbTree q t
  is <- getRootStates n d g
  simulate' is pt g

-- This is the heart of the simulation. Take a tree and a list of root states.
-- Recursively jump down the branches to the leafs.
simulate' ::
  (PrimMonad m) =>
  [State] ->
  Tree ProbMatrix ->
  Gen (PrimState m) ->
  m (Tree [State])
simulate' is (Node p f) g = do
  is' <- mapM (\i -> jump i p g) is
  f' <- sequence [simulate' is' t g | t <- f]
  return $ Node is' f'

toProbTreeMixtureModel ::
  V.Vector RateMatrix -> Tree Double -> Tree (V.Vector ProbMatrix)
toProbTreeMixtureModel qs =
  -- XXX: This function is slow. Parallelization?
  fmap (\a -> V.map (`probMatrix` a) qs)

getComponentsAndRootStates ::
  PrimMonad m =>
  Int ->
  V.Vector Double ->
  V.Vector StationaryDistribution ->
  Gen (PrimState m) ->
  m ([Int], [State])
getComponentsAndRootStates n ws ds g = do
  cs <- replicateM n $ categorical ws g
  is <- sequence [categorical (ds V.! c) g | c <- cs]
  return (cs, is)

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. Forget states at internal nodes. See also
-- 'simulateAndFlatten'.
simulateAndFlattenMixtureModel ::
  PrimMonad m =>
  Int ->
  V.Vector Double ->
  V.Vector StationaryDistribution ->
  V.Vector ExchangeabilityMatrix ->
  Tree Double ->
  Gen (PrimState m) ->
  -- | (IndicesOfComponents, [SimulatedSequenceForEachTip])
  m ([Int], [[State]])
simulateAndFlattenMixtureModel n ws ds es t g = do
  let qs = V.zipWith fromExchangeabilityMatrix es ds
      pt = toProbTreeMixtureModel qs t
  (cs, is) <- getComponentsAndRootStates n ws ds g
  ss <- simulateAndFlattenMixtureModel' is cs pt g
  return (cs, ss)

simulateAndFlattenMixtureModel' ::
  (PrimMonad m) =>
  [State] ->
  [Int] ->
  Tree (V.Vector ProbMatrix) ->
  Gen (PrimState m) ->
  m [[State]]
simulateAndFlattenMixtureModel' is cs (Node ps f) g = do
  is' <- sequence [jump i (ps V.! c) g | (i, c) <- zip is cs]
  if null f
    then return [is']
    else
      concat
        <$> sequence [simulateAndFlattenMixtureModel' is' cs t g | t <- f]

getChunks :: Int -> Int -> [Int]
getChunks c n = ns
  where
    n' = n `div` c
    r = n `mod` c
    ns = replicate r (n' + 1) ++ replicate (c - r) n'

splitGen :: PrimMonad m => Int -> Gen (PrimState m) -> m [Gen (PrimState m)]
splitGen n gen
  | n <= 0 = return []
  | otherwise = do
    seeds :: [V.Vector Word32] <- replicateM (n -1) $ uniformVector gen 256
    fmap (gen :) (mapM initialize seeds)

parComp :: Int -> (Int -> GenIO -> IO b) -> GenIO -> IO [b]
parComp num fun gen = do
  ncap <- getNumCapabilities
  let chunks = getChunks ncap num
  gs <- splitGen ncap gen
  mapConcurrently (uncurry fun) (zip chunks gs)

-- | See 'simulateAndFlattenMixtureModel', parallel version.
simulateAndFlattenMixtureModelPar ::
  Int ->
  V.Vector Double ->
  V.Vector StationaryDistribution ->
  V.Vector ExchangeabilityMatrix ->
  Tree Double ->
  GenIO ->
  IO ([Int], [[State]])
simulateAndFlattenMixtureModelPar n ws ds es t g = do
  let qs = V.zipWith fromExchangeabilityMatrix es ds
      pt = toProbTreeMixtureModel qs t
  -- The concurrent computation returns a list of ([Int], [[State]]) objects.
  -- They have to be concatenated horizontally.
  csss <-
    parComp
      n
      ( \n' g' ->
          do
            (cs, is) <- getComponentsAndRootStates n' ws ds g'
            ss <- simulateAndFlattenMixtureModel' is cs pt g'
            return (cs, ss)
      )
      g
  return (concatMap fst csss, horizontalConcat $ map snd csss)

-- | Simulate a number of sites for a given set of substitution models with
-- corresponding weights. Keep states at internal nodes. See also
-- 'simulate'.
simulateMixtureModel ::
  PrimMonad m =>
  Int ->
  V.Vector Double ->
  V.Vector StationaryDistribution ->
  V.Vector ExchangeabilityMatrix ->
  Tree Double ->
  Gen (PrimState m) ->
  m (Tree [State])
simulateMixtureModel n ws ds es t g = do
  let qs = V.zipWith fromExchangeabilityMatrix es ds
      pt = toProbTreeMixtureModel qs t
  (cs, is) <- getComponentsAndRootStates n ws ds g
  simulateMixtureModel' is cs pt g

-- See 'simulateAlongProbTree', only we have a number of mixture components. The
-- starting states and the components for each site have to be provided.
simulateMixtureModel' ::
  (PrimMonad m) =>
  [State] ->
  [Int] ->
  Tree (V.Vector ProbMatrix) ->
  Gen (PrimState m) ->
  m (Tree [State])
simulateMixtureModel' is cs (Node ps f) g = do
  is' <- sequence [jump i (ps V.! c) g | (i, c) <- zip is cs]
  f' <- sequence [simulateMixtureModel' is' cs t g | t <- f]
  return $ Node is' f'
