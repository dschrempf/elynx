-- |
-- Module      :  ELynx.Tools.Numeric
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Feb 14 13:28:37 2019.
--
-- Numerical functions.
module ELynx.Tools.Numeric
  ( -- * Numeric
    harmonic,
    roundN,
  )
where

-- | Calculate the nth harmonic number.
harmonic :: Int -> Double
harmonic 1 = 1.0
harmonic n = 1.0 / fromIntegral n + harmonic (n - 1)

-- | Round double to a given precision.
roundN :: Int -> Double -> Double
roundN n v = fromInteger (round $ v * (10 ^ n)) / (10.0 ^^ n)
