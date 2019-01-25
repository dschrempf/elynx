{- |
Module      :  EvoMod.Data.RateMatrix.RateMatrixSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 25 16:47:28 2019.

-}

module EvoMod.Data.RateMatrix.RateMatrixSpec
  (spec) where

import           Data.Vector.Generic
import           Test.Hspec

import           EvoMod.Data.RateMatrix.Nucleotide
import           EvoMod.Data.RateMatrix.RateMatrix
import           EvoMod.Tools                      (nearlyEqVec)

stationaryDist :: StationaryDist
stationaryDist = fromList [0.2, 0.3, 0.3, 0.2]

hkyRM :: RateMatrix
hkyRM = hky 6.0 stationaryDist

spec :: Spec
spec =
  describe "getStationaryDistribution" $
  it "extracts the stationary distribution from a rate matrix" $ do
  let sd = getStationaryDistribution hkyRM
  sd `shouldNotBe` Nothing
  nearlyEqVec <$> sd <*> Just stationaryDist `shouldBe` Just True
