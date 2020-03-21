{- |
Module      :  ELynx.Tools.LinearAlgebra
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:33:13 2019.

Tools for matrices from 'Numeric.LinearAlgebra'.

-}

module ELynx.Tools.LinearAlgebra
  (
    -- * Linear Algebra
    matrixSeparateSymSkew
  , matrixSetDiagToZero
  , dispv
  , dispm
  , dispmi
  )
where

import           Data.List

import           Numeric.LinearAlgebra

-- | Separate a square matrix into a symmetric and a skew-symmetric matrix.
matrixSeparateSymSkew :: Matrix R -> (Matrix R, Matrix R)
matrixSeparateSymSkew m = (mSym, mSkew)
 where
  trM   = tr m
  mSym  = scale 0.5 $ m + trM
  mSkew = scale 0.5 $ m - trM

-- | Set the diagonal entries of a matrix to zero.
matrixSetDiagToZero :: Matrix R -> Matrix R
matrixSetDiagToZero m = m - diag (takeDiag m)

-- | Display a vector with given precision.
dispv :: Int -> Vector R -> String
dispv p v = head $ tail $ lines $ dispf p (asRow v)

-- | Display a matrix with given precision.
dispm :: Int -> Matrix R -> String
dispm p m = intercalate "\n" $ init $ lines $ dispf p m

-- | Display a matrix with given precision and indent.
dispmi :: Int -> Int -> Matrix R -> String
dispmi p i m =
  intercalate "\n" $ map (replicate i ' ' ++) $ tail $ lines $ dispf p m
