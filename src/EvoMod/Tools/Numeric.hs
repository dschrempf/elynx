{- |
Module      :  EvoMod.Tools.Numeric
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:28:37 2019.

Numerical functions.

-}

module EvoMod.Tools.Numeric
  ( harmonic
  , xLogX
  ) where

import           EvoMod.Definitions

-- | Calculate the nth harmonic number.
harmonic :: Int -> Double
harmonic 1 = 1.0
harmonic n = 1.0 / fromIntegral n + harmonic (n-1)


-- | Calculate x*log(x) but set to 0.0 when x is smaller than 'eps'.
xLogX :: Double -> Double
xLogX x | x < 0.0   = error "Argument lower than zero."
        | x < eps   = 0.0
        | otherwise = x * log x
