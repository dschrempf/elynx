-- |
-- Module      :  ELynx.Data.MarkovProcess.NucleotideSpec
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Jan 25 16:47:28 2019.
module ELynx.Data.MarkovProcess.NucleotideSpec
  ( spec,
  )
where

import Data.Vector.Generic
import ELynx.Data.MarkovProcess.Nucleotide
import ELynx.Data.MarkovProcess.RateMatrix
import ELynx.Data.MarkovProcess.SubstitutionModel
import Test.Hspec

stationaryDist :: StationaryDistribution
stationaryDist = fromList [0.2, 0.3, 0.3, 0.2]

hkyModel :: SubstitutionModel
hkyModel = hky 6.0 stationaryDist

spec :: Spec
spec =
  describe "getStationaryDistribution" $
    it "extracts the stationary distribution from a rate matrix" $
      do
        let sd = getStationaryDistribution (rateMatrix hkyModel)
        sd `nearlyEqVec` stationaryDist `shouldBe` True
