{- |
Module      :  EvoMod.Data.MarkovProcess.EDMModel
Description :  Empirical distribution mixture models
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 19:57:55 2019.

Empricial distribution mixture (EDM) models are mixture models that share the
same exchangeability matrix but have different stationary distributions obtained
from data.

-}

module EvoMod.Data.MarkovProcess.EDMModel
  ( EDMComponent (..)
  , summarizeEDMComponents
  , edmModel
  ) where

import qualified Data.ByteString.Lazy.Char8                  as B

import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | Empirical distribution mixture model component.
data EDMComponent = EDMComponent
  { cWeight                 :: Double
  , cStationaryDistribution :: StationaryDistribution
  }
  deriving (Show, Eq)

-- | Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> B.ByteString
summarizeEDMComponents cs = B.pack
                            $ "Empiricial distribution mixture model with "
                            ++ show (length cs) ++ " components"

-- | Take a substitution model and mixture components to create an empirical
-- distribution mixture model (EDM model).
edmModel :: SubstitutionModel -> [EDMComponent] -> MixtureModel
edmModel sm cs = MixtureModel name
  [ MixtureModelComponent w sm {smStationaryDistribution = sd} | (EDMComponent w sd) <- cs ]
  where name = B.pack $ "EDM" ++ show (length cs)
