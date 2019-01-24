{-# LANGUAGE BangPatterns #-}
{- |
Module      :  EvoMod.Simulate.MarkovChain
Description :  Markov chain helpers
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 24 09:02:25 2019.

-}

module EvoMod.Simulate.MarkovChain
  ( ProbMatrix
  , State
  , probMatrix
  , jump
  ) where

import           Control.Monad.Primitive
-- import           Foreign.Storable
-- import           Foreign.Storable.Tuple ()
import           Numeric.LinearAlgebra
import           System.Random.MWC
-- import           System.Random.MWC.CondensedTable
import           System.Random.MWC.Distributions

-- | A probability matrix, P_ij(t) = Pr (X_t = j | X_0 = i).
type ProbMatrix = Matrix R

-- | Make type signatures a little clearer.
type State = Int

-- | The important matrix that gives the probabilities to move from one state to
-- another in a specific time (branch length).
probMatrix :: Matrix R -> Double -> ProbMatrix
probMatrix q t | t == 0 = if rows q == cols q
                          then ident (rows q)
                          else error "Matrix is not square."
               | t < 0 = error "Time is negative."
               | otherwise = expm $ scale t q

-- TODO: Write storable instance, compilation is really slow otherwise.
-- instance Storable (Int, R) where
--   sizeOf (x, y) = sizeOf x + sizeOf y

-- | Move from a given state to a new one according to a transition probability
-- matrix (for performance reasons this probability matrix needs to be given as
-- a list of generators, see
-- https://hackage.haskell.org/package/distribution-1.1.0.0/docs/Data-Distribution-Sample.html).
-- This function is the bottleneck of the simulator and takes up most of the
-- computation time. However, I was not able to find a faster implementation
-- than the one from Data.Distribution.
--
-- -- TODO: Do not generate table for each jump.
-- jump :: (PrimMonad m) => State -> ProbMatrix -> Gen (PrimState m) -> m State
-- jump i p = genFromTable table
--   where
--     ws = toList $ p ! i
--     vsAndWs = fromList [ (v, w) | (v, w) <- zip [(0 :: Int) ..] ws
--                                 , w > 0 ]
--     table = tableFromProbabilities vsAndWs
jump :: (PrimMonad m) => State -> ProbMatrix -> Gen (PrimState m) -> m State
jump i p = categorical (p ! i)

-- -- | Perform N jumps from a given state and according to a transition
-- -- probability matrix transformed to a list of generators. This implementation
-- -- uses 'foldM' and I am not sure how to access or store the actual chain. This
-- -- could be done by an equivalent of 'scanl' for general monads, which I was
-- -- unable to find. This function is neat, but will most likely not be needed.
-- -- However, it is instructive and is left in place.
-- jumpN :: (MonadRandom m) => State -> [Generator State] -> Int -> m State
-- jumpN s p n = foldM jump s (replicate n p)
