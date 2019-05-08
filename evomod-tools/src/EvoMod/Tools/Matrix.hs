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
    fMapRowSeq
  , fMapRowPar
  , fMapRowParChunk
  , fMapColSeq
  , fMapColPar
  , fMapColParChunk
  , (|||)
  , (===)
  , subSampleMatrix
  ) where

import           Control.Parallel.Strategies
import           Data.List
import qualified Data.Matrix.Storable        as M
import qualified Data.Vector.Storable        as V

-- | Map a function on each row of a DIM2 array; sequential version.
fMapRowSeq :: (V.Storable a, V.Storable b) => (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
fMapRowSeq f m = M.fromRows $ map f (M.toRows m)

-- | Map a function on each row of a DIM2 array; parallel version.
fMapRowPar :: (V.Storable a, V.Storable b) => (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
fMapRowPar f m = M.fromRows $ parMap rseq f (M.toRows m)

-- | Map a function on each row of a DIM2 array; parallel version with given chunk size.
fMapRowParChunk :: (V.Storable a, V.Storable b) => Int -> (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
fMapRowParChunk n f m = M.fromRows (map f (M.toRows m) `using` parListChunk n rseq)

-- | Map a function on each row of a DIM2 array; sequential version.
fMapColSeq :: (V.Storable a, V.Storable b) => (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
fMapColSeq f m = M.fromColumns $ map f (M.toColumns m)

-- | Map a function on each row of a DIM2 array; parallel version.
fMapColPar :: (V.Storable a, V.Storable b) => (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
fMapColPar f m = M.fromColumns $ parMap rseq f (M.toColumns m)

-- | Map a function on each row of a DIM2 array; parallel version with given chunk size.
fMapColParChunk :: (V.Storable a, V.Storable b) => Int -> (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
fMapColParChunk n f m = M.fromColumns (map f (M.toColumns m) `using` parListChunk n rseq)

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

-- Sample the given sites from a matrix.
subSampleMatrix :: (V.Storable a) => [Int] -> M.Matrix a -> M.Matrix a
subSampleMatrix is m = M.fromColumns $ foldl' (\a i -> M.takeColumn m i : a) [] is
