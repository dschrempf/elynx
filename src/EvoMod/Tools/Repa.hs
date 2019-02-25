{- |
Module      :  EvoMod.Tools.Repa
Description :  Tools for repa arrays
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon Feb 25 15:26:48 2019.

-}

module EvoMod.Tools.Repa
  ( nRows
  , nCols
  , nThRow
  , nThCol
  , fMapRow
  , fMapCol
  ) where

import Prelude as P
import           Data.Array.Repa as R
import           Data.Array.Repa.Algorithms.Matrix

-- | From Data.Array.Repa.Algorithms.Matrix of repa-algorithms.
nRows :: Source r e => Array r DIM2 e -> Int
nRows = col . extent

-- | Get number of columns from a shape. See 'row' of repa-algorithms.
nCols :: Source r e => Array r DIM2 e -> Int
nCols = row . extent

-- | Select nth row of matrix.
nThRow :: Source r e => Int -> Array r DIM2 e -> Array D DIM1 e
nThRow n arr = slice arr (Z :. All :. n)

-- | Select nth column of matrix.
nThCol :: Source r e => Int -> Array r DIM2 e -> Array D DIM1 e
nThCol n arr = slice arr (Z :. n :. All)

-- | Map a function on each row of a DIM2 array.
fMapRow :: Source r e => (Array D DIM1 e -> b) -> Array r DIM2 e -> [b]
fMapRow f arr = P.map (\n -> f (nThRow n arr)) [0 .. nRows arr - 1]

-- | Map a function on each row of a DIM2 array.
fMapCol :: Source r e => (Array D DIM1 e -> b) -> Array r DIM2 e -> [b]
fMapCol f arr = P.map (\n -> f (nThCol n arr)) [0 .. nCols arr - 1]
