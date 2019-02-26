{-# LANGUAGE MultiParamTypeClasses #-}

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

import           Data.Array.Repa                   as R
import           Data.Array.Repa.Algorithms.Matrix
import           Data.Vector.Storable              as V
import           Prelude                           as P

-- | From Data.Array.Repa.Algorithms.Matrix of repa-algorithms.
nRows :: Source r e => Array r DIM2 e -> Int
nRows = row . extent

-- | Get number of columns from a shape. See 'row' of repa-algorithms.
nCols :: Source r e => Array r DIM2 e -> Int
nCols = col . extent

-- | Select nth row of matrix.
nThRow :: Source r e => Int -> Array r DIM2 e -> Array D DIM1 e
nThRow n arr = R.slice arr (Z :. n :. All)

-- | Select nth column of matrix.
nThCol :: Source r e => Int -> Array r DIM2 e -> Array D DIM1 e
nThCol n arr = R.slice arr (Z :. All :. n)

-- | Map a function on each row of a DIM2 array.
fMapRow :: (Source r e, V.Storable b) => (Array D DIM1 e -> b) -> Array r DIM2 e -> V.Vector b
fMapRow f arr = generate (nRows arr) (\n -> f (nThRow n arr))

-- | Map a function on each row of a DIM2 array.
fMapCol :: (Source r e, V.Storable b) => (Array D DIM1 e -> b) -> Array r DIM2 e -> V.Vector b
fMapCol f arr = generate (nCols arr) (\n -> f (nThCol n arr))
