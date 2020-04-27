{- |
Module      :  ELynx.Tools.Vector
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:33:13 2019.

Tools for vectors from 'Data.Vector.Generic'.

-}

module ELynx.Tools.Vector
  ( -- * Vectors
    sumVec
  , normalizeSumVec
  , uniformVec
  , meanVec
  , chopVec
  , randomInsertVec
  )
where

import           Control.Monad.Primitive
import qualified Data.Vector.Generic           as V
import           System.Random.MWC

-- | Sum of elements.
sumVec :: (Num a, V.Vector v a) => v a -> a
sumVec = V.foldl' (+) 0

-- | Normalize a vector such that elements sum to a given value.
normalizeSumVec :: (Fractional a, V.Vector v a) => a -> v a -> v a
normalizeSumVec c v = V.map (* c') v
 where
  s  = sumVec v
  c' = c / s

-- | A uniform vector of given length.
uniformVec :: (Fractional a, V.Vector v a) => Int -> v a
uniformVec n = V.replicate n (1 / fromIntegral n)

-- | Mean of a vector.
meanVec :: (Fractional a, V.Vector v a) => v a -> a
meanVec v = sumVec v / fromIntegral (V.length v)

-- | Chop list into chunks of given length. If the last chop is shorter than
-- length, it is dropped.
chopVec :: V.Vector v a => Int -> v a -> [v a]
chopVec n xs | V.length xs < n = []
             | otherwise       = V.take n xs : chopVec n (V.drop n xs)

-- | Insert element into random position of vector.
randomInsertVec
  :: (PrimMonad m, V.Vector v a) => a -> v a -> Gen (PrimState m) -> m (v a)
randomInsertVec e v g = do
  let l = V.length v
  i <- uniformR (0, l) g
  return $ V.take i v V.++ V.singleton e V.++ V.drop i v

