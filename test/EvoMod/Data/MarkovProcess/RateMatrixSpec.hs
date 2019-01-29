{- |
Module      :  EvoMod.Data.MarkovProcess.RateMatrixSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 25 16:47:28 2019.

-}

module EvoMod.Data.MarkovProcess.RateMatrixSpec
  (spec) where

import           Data.Vector.Generic
import           Test.Hspec

import           EvoMod.Data.MarkovProcess.Nucleotide
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Tools                      (nearlyEqVec)

stationaryDist :: StationaryDistribution
stationaryDist = fromList [0.2, 0.3, 0.3, 0.2]

hkyRM :: RateMatrix
hkyRM = hky 6.0 stationaryDist

spec :: Spec
spec =
  describe "getStationaryDistribution" $
  it "extracts the stationary distribution from a rate matrix" $ do
  let sd = getStationaryDistribution hkyRM
  sd `nearlyEqVec` stationaryDist `shouldBe` True
