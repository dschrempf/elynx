{-# LANGUAGE FlexibleContexts #-}

{- |
Module      :  EvoMod.Tools.UVector
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:33:13 2019.

Tools for vectors from 'Data.Vector.Storable'.

-}

module EvoMod.Tools.UVector
  (
    sumVec
  , normalizeSumVec
  , uniformVec
  , meanVec
  ) where

import  qualified         Data.Vector.Unboxed as V

-- | Sum of elements.
sumVec :: (Num a, V.Unbox a) => V.Vector a -> a
sumVec = V.foldl' (+) 0

-- | Normalize a vector such that elements sum to a given value.
normalizeSumVec :: Double -> V.Vector Double -> V.Vector Double
normalizeSumVec c v = V.map (* c') v
  where s = sumVec v
        c' = c/s

-- | A uniform vector of given length.
uniformVec :: Int -> V.Vector Double
uniformVec n = V.replicate n (1 / fromIntegral n)

-- | Mean of a vector.
meanVec :: V.Vector Double -> Double
meanVec v = sumVec v / fromIntegral (V.length v)
