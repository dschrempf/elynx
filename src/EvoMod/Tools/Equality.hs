{- |
Module      :  EvoMod.Tools.Equality
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:27:05 2019.

Equality tests.

-}

module EvoMod.Tools.Equality
  (
    allEqual
  , nearlyEqWith
  , nearlyEq
  , nearlyEqVecWith
  , nearlyEqVec
  , nearlyEqMatWith
  , nearlyEqMat
  ) where

import           EvoMod.Definitions
import           Numeric.LinearAlgebra

-- | Test if all elements of a list are equal.
allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

-- | Test for equality with given tolerance (needed because of machine precision).
nearlyEqWith :: Double -> Double -> Double -> Bool
nearlyEqWith tol a b = tol > abs (a-b)

-- | Test for equality with predefined tolerance (needed because of machine precision).
nearlyEq :: Double -> Double -> Bool
nearlyEq = nearlyEqWith eps

-- Test if the given number is nearly equal to all elements of a vector.
nearlyEqValListWith :: Double -> Double -> [Double] -> Bool
nearlyEqValListWith tol a = all (nearlyEqWith tol a)

-- | Test if two vectors are nearly equal.
nearlyEqVecWith :: Double -> Vector R -> Vector R -> Bool
nearlyEqVecWith tol a b = nearlyEqValListWith tol 0 (toList $ a - b)

-- | Test if two vectors are nearly equal.
nearlyEqVec :: Vector R -> Vector R -> Bool
nearlyEqVec = nearlyEqVecWith eps

-- | Test if two vectors are nearly equal.
nearlyEqMatWith :: Double -> Matrix R -> Matrix R -> Bool
nearlyEqMatWith tol a b = nearlyEqValListWith tol 0 (concat . toLists $ a - b)

-- | Test if two vectors are nearly equal.
nearlyEqMat :: Matrix R -> Matrix R -> Bool
nearlyEqMat = nearlyEqMatWith eps
