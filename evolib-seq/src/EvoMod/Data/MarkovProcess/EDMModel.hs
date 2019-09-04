{- |
Module      :  EvoMod.Data.MarkovProcess.EDMModel
Description :  Empiricial distribution mixture models
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Mar 19 13:25:55 2019.

-}

module EvoMod.Data.MarkovProcess.EDMModel
  (
    EDMComponent
  , edmModel
  ) where

import qualified Data.Vector.Storable                        as V

import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | An empirical mixture model component has a weight and a stationary
-- distribution.
type EDMComponent = (Weight, V.Vector Double)

-- | Create an EDM model from components and a substitution module construction
-- manual from stationary distributions.
edmModel :: [EDMComponent] -> (StationaryDistribution -> SubstitutionModel)
         -> MixtureModel
edmModel cs f =
  MixtureModel n [ Component w (f d)
                 | (w, d) <- cs ]
  where n = "EDM" ++ show (length cs)
