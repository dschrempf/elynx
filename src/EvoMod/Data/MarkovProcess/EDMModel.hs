{- |
Module      :  EvoMod.Data.MarkovProcess.EDMModel
Description :  Empirical distribution mixture models
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 19:57:55 2019.

-}

module EvoMod.Data.MarkovProcess.EDMModel
  ( EDMComponent (..)
  ) where

-- import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.RateMatrix
-- import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | Empirical distribution mixture model component.
data EDMComponent = EDMComponent
  { cWeight                 :: Double
  , cStationaryDistribution :: StationaryDistribution
  }
  deriving (Show, Eq)

-- -- | Take a substitution model and
-- edmModel :: SubstitutionModel -> [EDMComponent] -> MixtureModel
-- edmModel sm cs = MixtureModel ("EDM" ++ show nc)
--   where es = smExchMatrix s
--         nc = length cs
