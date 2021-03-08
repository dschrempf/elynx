-- |
-- Module      :  Length
-- Description :  Benchmark length newtype
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Nov  3 13:42:00 2020.
module Length
  ( lengthSumFoldl',
    lengthSumFoldl'Unsafe,
    lengthSumFoldl'NumInstance,
    doubleSumFoldl',
    doubleSum,
  )
where

import Data.Foldable
import ELynx.Tree.Length

lengthSumFoldl' :: [Length] -> Length
lengthSumFoldl' =
  foldl'
    ( \x y ->
        either (error . ("lengthSumFoldl'" ++)) id $
          toLength $
            fromLength x + fromLength y
    )
    0

lengthSumFoldl'Unsafe :: [Length] -> Length
lengthSumFoldl'Unsafe = foldl' (\x y -> toLengthUnsafe $ fromLength x + fromLength y) 0

lengthSumFoldl'NumInstance :: [Length] -> Length
lengthSumFoldl'NumInstance = foldl' (+) 0

doubleSumFoldl' :: [Double] -> Double
doubleSumFoldl' = foldl' (+) 0

doubleSum :: [Double] -> Double
doubleSum = foldl' (+) 0
