-- |
-- Module      :  ELynx.Tools.Vector
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Feb 14 13:33:13 2019.
--
-- Tools for vectors from 'Data.Vector.Generic'.
module ELynx.Tools.Vector
  ( -- * Vectors
    normalizeSumVec,
    meanVec,
    randomInsertVec,
  )
where

import Control.Monad.Primitive
import qualified Data.Vector.Generic as V
import System.Random.MWC

-- | Normalize a vector such that elements sum to a given value.
normalizeSumVec :: (Fractional a, V.Vector v a) => a -> v a -> v a
normalizeSumVec c v = V.map (* c') v
  where
    s = V.sum v
    c' = c / s

-- | Mean of a vector.
meanVec :: (Fractional a, V.Vector v a) => v a -> a
meanVec v = V.sum v / fromIntegral (V.length v)

-- | Insert element into random position of vector.
randomInsertVec ::
  (PrimMonad m, V.Vector v a) => a -> v a -> Gen (PrimState m) -> m (v a)
randomInsertVec e v g = do
  let l = V.length v
  i <- uniformR (0, l) g
  return $ V.take i v V.++ V.singleton e V.++ V.drop i v
