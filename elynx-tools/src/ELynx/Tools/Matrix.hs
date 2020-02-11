{- |
Module      :  ELynx.Tools.Matrix
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:33:13 2019.

Tools for matrices from 'Numeric.LinearAlgebra'.

-}

module ELynx.Tools.Matrix
  ( fMapRowSeq
  , fMapRowPar
  , fMapRowParChunk
  , fMapColSeq
  , fMapColPar
  , fMapColParChunk
  , (|||)
  , (===)
  , subSampleMatrix
  )
where

import           Control.Parallel.Strategies
import           Data.List
import qualified Data.Matrix.Generic           as M
import qualified Data.Vector.Generic           as V

-- | Map a function on each row of a DIM2 array; sequential version.
fMapRowSeq
  :: (V.Vector v a, V.Vector v b)
  => (v a -> v b)
  -> M.Matrix v a
  -> M.Matrix v b
fMapRowSeq f m = M.fromRows $ map f (M.toRows m)

-- | Map a function on each row of a DIM2 array; parallel version.
fMapRowPar
  :: (V.Vector v a, V.Vector v b)
  => (v a -> v b)
  -> M.Matrix v a
  -> M.Matrix v b
fMapRowPar f m = M.fromRows $ parMap rseq f (M.toRows m)

-- | Map a function on each row of a DIM2 array; parallel version with given chunk size.
fMapRowParChunk
  :: (V.Vector v a, V.Vector v b)
  => Int
  -> (v a -> v b)
  -> M.Matrix v a
  -> M.Matrix v b
fMapRowParChunk n f m =
  M.fromRows (map f (M.toRows m) `using` parListChunk n rseq)

-- | Map a function on each row of a DIM2 array; sequential version.
fMapColSeq
  :: (V.Vector v a, V.Vector v b)
  => (v a -> v b)
  -> M.Matrix v a
  -> M.Matrix v b
fMapColSeq f m = M.fromColumns $ map f (M.toColumns m)

-- | Map a function on each row of a DIM2 array; parallel version.
fMapColPar
  :: (V.Vector v a, V.Vector v b)
  => (v a -> v b)
  -> M.Matrix v a
  -> M.Matrix v b
fMapColPar f m = M.fromColumns $ parMap rseq f (M.toColumns m)

-- | Map a function on each row of a DIM2 array; parallel version with given chunk size.
fMapColParChunk
  :: (V.Vector v a, V.Vector v b)
  => Int
  -> (v a -> v b)
  -> M.Matrix v a
  -> M.Matrix v b
fMapColParChunk n f m =
  M.fromColumns (map f (M.toColumns m) `using` parListChunk n rseq)

-- | Horizontal concatenation.
(|||) :: (V.Vector v a) => M.Matrix v a -> M.Matrix v a -> M.Matrix v a
(|||) l r = M.fromColumns $ lCs ++ rCs
 where
  lCs = M.toColumns l
  rCs = M.toColumns r

-- | Vertical concatenation.
(===) :: (V.Vector v a) => M.Matrix v a -> M.Matrix v a -> M.Matrix v a
(===) l r = M.fromRows $ lRs ++ rRs
 where
  lRs = M.toRows l
  rRs = M.toRows r

-- | Sample the given sites from a matrix.
subSampleMatrix :: (V.Vector v a) => [Int] -> M.Matrix v a -> M.Matrix v a
subSampleMatrix is m =
  M.fromColumns $ foldl' (\a i -> M.takeColumn m i : a) [] is
