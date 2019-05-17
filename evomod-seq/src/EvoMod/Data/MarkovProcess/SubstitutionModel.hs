{-# LANGUAGE TemplateHaskell #-}

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
  ( Name
  , Params
  , SubstitutionModel
  , code
  , name
  , substitutionModel
  , substitutionModelUnnormalized
  , smStationaryDistribution
  , smExchangeabilityMatrix
  , scaleSubstitutionModel
  , totalRateSubstitutionModel
  , normalizeSubstitutionModel
  , appendNameSubstitutionModel
  , summarizeSubstitutionModel
  , getRateMatrix
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8           as L
import           Numeric.LinearAlgebra                hiding ((<>))

import qualified EvoMod.Data.Alphabet.Character       as C
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Tools.Definitions
import           EvoMod.Tools.LinearAlgebra
import           EvoMod.Tools.Numeric

-- | Name of substitution model; abstracted and subject to change.
type Name = String

-- | Parameters of substitution model. May be the empty list.
type Params = [Double]

-- | Complete definition of a substitution model. Create instances with
-- 'substitutionModel'.
data SubstitutionModel = SubstitutionModel
  { _code                   :: C.Code
  , _name                   :: Name
  , _params                 :: Params
  , _stationaryDistribution :: StationaryDistribution
  , _exchangeabilityMatrix  :: ExchangeabilityMatrix
  }
  deriving (Show, Read)

makeLenses ''SubstitutionModel

-- | Create normalized 'SubstitutionModel'. See 'normalizeSubstitutionModel'.
substitutionModel :: C.Code -> Name -> Params
                  -> StationaryDistribution -> ExchangeabilityMatrix
                  -> SubstitutionModel
substitutionModel c n ps d e = normalizeSubstitutionModel $ SubstitutionModel c n ps d e

-- | Create UNNORMALIZED 'SubstitutionModel'. See 'substitutionModel'.
substitutionModelUnnormalized :: C.Code -> Name -> Params
                  -> StationaryDistribution -> ExchangeabilityMatrix
                  -> SubstitutionModel
substitutionModelUnnormalized = SubstitutionModel

-- This is annoying, but this is the easiest way to provide Haddock comments.
-- Another way would be to use 'makeLensesWith' and a custom 'LensRules' that
-- does not create type signatures. Then create the type signature manually and
-- document them.

-- | Access stationary distribution.
smStationaryDistribution :: Lens' SubstitutionModel StationaryDistribution
smStationaryDistribution = stationaryDistribution

-- | Access exchangeability matrix.
smExchangeabilityMatrix :: Lens' SubstitutionModel ExchangeabilityMatrix
smExchangeabilityMatrix = exchangeabilityMatrix

-- | Scale the rate of a substitution model by given factor.
scaleSubstitutionModel :: Double -> SubstitutionModel -> SubstitutionModel
scaleSubstitutionModel r = over exchangeabilityMatrix (scale r)

-- | Get scale of substitution model.
totalRateSubstitutionModel :: SubstitutionModel -> Double
totalRateSubstitutionModel sm = totalRate (sm ^. stationaryDistribution) (getRateMatrix sm)

-- | Normalize a substitution model, so that, on average, one substitution
-- happens per unit time.
normalizeSubstitutionModel :: SubstitutionModel -> SubstitutionModel
normalizeSubstitutionModel sm = scaleSubstitutionModel (1.0/r) sm
  where m = getRateMatrix sm
        r = totalRate (sm ^. stationaryDistribution) m

-- | Abbend to name.
appendNameSubstitutionModel :: Name -> SubstitutionModel -> SubstitutionModel
appendNameSubstitutionModel n = over name (<> n)

-- | Summarize a substitution model; lines to be printed to screen or log.
summarizeSubstitutionModel :: SubstitutionModel -> [L.ByteString]
summarizeSubstitutionModel sm = map L.pack $
  (show (sm ^. code) ++ " substitution model: " ++ sm ^. name ++ ".") :
  [ "Parameters: " ++ show (sm ^. params) ++ "." | not (null (sm ^. params))] ++
  case sm ^. code of
    C.DNA -> [ "Stationary distribution: " ++ dispv precision (sm ^. stationaryDistribution) ++ "."
           , "Exchangeability matrix:\n" ++ dispmi 2 precision (sm ^. exchangeabilityMatrix) ++ "."
           , "Scale: " ++ show (roundN precision $ totalRateSubstitutionModel sm) ++ "."
           ]
    C.Protein -> [ "Stationary distribution: " ++ dispv precision (sm ^. stationaryDistribution) ++ "."
               , "Scale: " ++ show (roundN precision $ totalRateSubstitutionModel sm) ++ "."
               ]
    _ -> error "Extended character sets are not supported with substitution models."

-- | Calculate rate matrix from substitution model.
getRateMatrix :: SubstitutionModel -> RateMatrix
getRateMatrix sm = fromExchangeabilityMatrix (sm ^. exchangeabilityMatrix) (sm ^. stationaryDistribution)
