{- |
Module      :  EvoMod.Tools.Definitions
Description :  Some global definitions
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 25 17:17:41 2019.

-}

module EvoMod.Tools.Definitions
  ( eps
  , precision
  ) where

-- | Required precision when comparing 'Double' values.
eps :: Double
eps = 1e-12

-- | Default output precision.
precision :: Int
precision = 3
