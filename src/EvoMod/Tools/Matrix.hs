{- |
Module      :  EvoMod.Tools.Matrix
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:33:13 2019.

Tools for matrices from 'Numeric.LinearAlgebra'.

-}

module EvoMod.Tools.Matrix
  (
    fMapRow
  , fMapCol
  , (|||)
  , (===)
  ) where

import qualified Data.Vector.Storable              as V
import qualified Data.Matrix.Storable              as M

-- | Map a function on each row of a DIM2 array.
fMapRow :: (V.Storable a, V.Storable b) => (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
fMapRow f m = M.fromRows $ map f (M.toRows m)

-- | Map a function on each row of a DIM2 array.
fMapCol :: (V.Storable a, V.Storable b) => (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
fMapCol f m = M.fromColumns $ map f (M.toColumns m)

-- | Horizontal concatenation.
(|||) :: (V.Storable a) => M.Matrix a -> M.Matrix a -> M.Matrix a
(|||) l r = M.fromColumns $ lCs ++ rCs
  where
    lCs = M.toColumns l
    rCs = M.toColumns r

-- | Vertical concatenation.
(===) :: (V.Storable a) => M.Matrix a -> M.Matrix a -> M.Matrix a
(===) l r = M.fromRows $ lRs ++ rRs
  where
    lRs = M.toRows l
    rRs = M.toRows r
