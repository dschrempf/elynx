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
  , edmModelGlobalNormalization
  ) where

import qualified Data.ByteString.Lazy.Char8                  as L
import qualified Data.Vector.Storable                        as V

import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | An empirical mixture model component has a weight and a stationary
-- distribution.
type EDMComponent = (Weight, V.Vector Double)

-- | Create an EDM model from components and a given substitution model in which
-- only the stationary distribution will be replaced for each mixture model
-- component. Each mixture model component is normalized separately.
edmModel :: [EDMComponent] -> (StationaryDistribution -> SubstitutionModel)
         -> MixtureModel
edmModel cs f =
  MixtureModel n [ MixtureModelComponent w (normalizeSubstitutionModel $ f d)
                 | (w, d) <- cs ]
  where n = L.pack $ "EDM" ++ show (length cs)

-- | See 'edmModel'; global normalization.
edmModelGlobalNormalization :: [EDMComponent] -> (StationaryDistribution -> SubstitutionModel)
                            -> MixtureModel
edmModelGlobalNormalization cs f = normalizeMixtureModel $
  MixtureModel n [ MixtureModelComponent w (f d)
                 | (w, d) <- cs ]
  where n = L.pack $ "EDM" ++ show (length cs) ++ " global normalization"
