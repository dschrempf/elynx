{- |
Module      :  EvoMod.Data.RateMatrix.EDMModel
Description :  Empirical distribution mixture models
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 12:16:13 2019.

-}

module EvoMod.Data.RateMatrix.EDMModel
  ( EDMComponent (..)
  , EDMComponents (..)
  , EDMModel (..)
  ) where

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.RateMatrix.RateMatrix

-- | An empirical distribution mixture component has a weight and a stationary
-- distribution.
data EDMComponent = EDMComponent Double StationaryDist
  deriving (Show, Eq)

-- | Number of components and the components itself.
data EDMComponents = EDMComponents Int [EDMComponent]
  deriving (Show, Eq)

-- | Fully specified empirical distribution mixture model. Consists of a 'Code'
-- type (e.g., 'Protein'), an exchangeability matrix and the mixture components.
data EDMModel = EDMModel
  { edmCode       :: Code
  , edmExchMatrix :: ExchMatrix
  , edmComponents :: EDMComponents
  }
  deriving (Show, Eq)
