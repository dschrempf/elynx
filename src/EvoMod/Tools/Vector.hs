{- |
Module      :  EvoMod.Tools.Vector
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:33:13 2019.

Tools for vectors from 'Data.Vector.Storable'.

-}

module EvoMod.Tools.Vector
  (
    sumVec
  , normalizeSumVec
  , uniformVec
  , meanVec
  ) where

import           Data.Vector.Storable as V

-- | Sum of elements.
sumVec :: (Num a, Storable a) => Vector a -> a
sumVec = V.foldl' (+) 0

-- | Normalize a vector such that elements sum to a given value.
normalizeSumVec :: Double -> Vector Double -> Vector Double
normalizeSumVec c v = V.map (* c') v
  where s = sumVec v
        c' = c/s

-- | A uniform vector of given length.
uniformVec :: Int -> Vector Double
uniformVec n = V.replicate n (1 / fromIntegral n)

-- | Mean of a vector.
meanVec :: Vector Double -> Double
meanVec v = sumVec v / fromIntegral (V.length v)
