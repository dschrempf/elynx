{- |
Module      :  EvoMod.Data.MarkovProcess.SubstitutionModel
Description :  Data type describing substitution model
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 19:10:46 2019.

-}

module EvoMod.Data.MarkovProcess.SubstitutionModel
  ( SubstitutionModel (..)
  ) where

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.RateMatrix

-- | Complete definition of a substitution model.
data SubstitutionModel = SubstitutionModel
  { mCode                   :: Code
  , mName                   :: String
  , mParams                 :: [Double]
  , mStationaryDistribution :: StationaryDistribution
  , mExchMatrix             :: ExchMatrix
  , mRateMatrix             :: RateMatrix
  }
