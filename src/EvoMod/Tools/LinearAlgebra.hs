{- |
Module      :  EvoMod.Tools.LinearAlgebra
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:33:13 2019.

Tools for vectors and matrices.

-}

module EvoMod.Tools.LinearAlgebra
  (
    -- * Vectors.
    normalizeSumVec
  , uniformVec
    -- * Matrices.
  , matrixSeparateSymSkew
  , matrixSetDiagToZero
  ) where

import           Numeric.LinearAlgebra

-- | Normalize a vector such that elements sum to a given value. See 'normalize' but with 1-norm.
normalizeSumVec :: Double -> Vector R -> Vector R
normalizeSumVec c v = v * scalar c'
  where s = sumElements v
        c' = c/s

-- | A uniform vector of given length.
uniformVec :: Int -> Vector R
uniformVec n = vector $ replicate n (1 / fromIntegral n)

-- | Separate a square matrix into a symmetric and a skew-symmetric matrix.
matrixSeparateSymSkew :: Matrix R -> (Matrix R, Matrix R)
matrixSeparateSymSkew m = (mSym, mSkew)
  where trM = tr m
        mSym  = scale 0.5 $ m + trM
        mSkew = scale 0.5 $ m - trM

-- | Set the diagonal entries of a matrix to zero.
matrixSetDiagToZero :: Matrix R -> Matrix R
matrixSetDiagToZero m = m - diag (takeDiag m)
