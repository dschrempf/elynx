{- |
Module      :  EvoMod.Tools.Vector
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:33:13 2019.

Tools for vectors from 'Data.Vector.Generic'.

-}

module EvoMod.Tools.Vector
  (
    sumVec
  , normalizeSumVec
  , uniformVec
  , meanVec
  , chop
  ) where

import qualified Data.Vector.Generic as V

-- | Sum of elements.
sumVec :: (Num a, V.Vector v a) => v a -> a
sumVec = V.foldl' (+) 0

-- | Normalize a vector such that elements sum to a given value.
normalizeSumVec :: (Fractional a, V.Vector v a) => a -> v a -> v a
normalizeSumVec c v = V.map (* c') v
  where s = sumVec v
        c' = c/s

-- | A uniform vector of given length.
uniformVec :: (Fractional a, V.Vector v a) => Int -> v a
uniformVec n = V.replicate n (1 / fromIntegral n)

-- | Mean of a vector.
meanVec :: (Fractional a, V.Vector v a) => v a -> a
meanVec v = sumVec v / fromIntegral (V.length v)

-- | Chop list into chunks of given length. If the last chop is shorter than
-- length, it is dropped.
chop :: V.Vector v a => Int -> v a -> [v a]
chop n xs | V.length xs < n = []
          | otherwise     = V.take n xs : chop n (V.drop n xs)

