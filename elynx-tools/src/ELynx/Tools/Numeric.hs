{- |
Module      :  ELynx.Tools.Numeric
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:28:37 2019.

Numerical functions.

-}

module ELynx.Tools.Numeric
  ( -- * Numeric
    harmonic
  , xLogX
  , roundN
  )
where

import           ELynx.Tools.Equality

-- | Calculate the nth harmonic number.
harmonic :: Int -> Double
harmonic 1 = 1.0
harmonic n = 1.0 / fromIntegral n + harmonic (n - 1)

-- | Calculate x*log(x) but set to 0.0 when x is smaller than 'eps'.
xLogX :: Double -> Double
xLogX x | x < 0.0          = error "Argument lower than zero."
        | x `nearlyEq` 0.0 = 0.0
        | otherwise        = x * log x

-- | Round double to a given precision.
roundN :: Int -> Double -> Double
roundN n v = fromInteger (round $ v * (10 ^ n)) / (10.0 ^^ n)
