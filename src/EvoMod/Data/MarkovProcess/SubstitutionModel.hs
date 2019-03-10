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
  ( SubstitutionModelName
  , SubstitutionModelParams
  , SubstitutionModel (SubstitutionModel)
  , smCode
  , smName
  , smStationaryDistribution
  , smExchangeabilityMatrix
  , scaleSubstitutionModel
  , renameSubstitutionModel
  , appendNameSubstitutionModel
  , summarizeSubstitutionModel
  , getRateMatrix
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8           as L
import           Numeric.LinearAlgebra                hiding ((<>))

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.RateMatrix

-- | Name of substitution model; abstracted and subject to change.
type SubstitutionModelName = L.ByteString

-- | Parameters of substitution model. May be the empty list.
type SubstitutionModelParams = [Double]

-- | Complete definition of a substitution model.
data SubstitutionModel = SubstitutionModel
  { _code                   :: Code
  , _name                   :: SubstitutionModelName
  , _params                 :: SubstitutionModelParams
  , _stationaryDistribution :: StationaryDistribution
  , _exchangeabilityMatrix  :: ExchangeabilityMatrix
  }
  deriving (Show, Read)

makeLenses ''SubstitutionModel

-- This is annoying, but this is the easiest way to provide Haddock comments.
-- Another way would be to use 'makeLensesWith' and a custom 'LensRules' that
-- does not create type signatures. The create the type signature manually and
-- document them.

-- | Access code.
smCode :: Lens' SubstitutionModel Code
smCode = code

-- | Access name.
smName :: Lens' SubstitutionModel SubstitutionModelName
smName = name

-- | Access stationary distribution.
smStationaryDistribution :: Lens' SubstitutionModel StationaryDistribution
smStationaryDistribution = stationaryDistribution

-- | Access exchangeability matrix.
smExchangeabilityMatrix :: Lens' SubstitutionModel ExchangeabilityMatrix
smExchangeabilityMatrix = exchangeabilityMatrix

-- | Scale the rate of a substitution model by given factor.
scaleSubstitutionModel :: Double -> SubstitutionModel -> SubstitutionModel
scaleSubstitutionModel s = over exchangeabilityMatrix (scale s)

-- | Rename a substitution model.
renameSubstitutionModel :: SubstitutionModelName -> SubstitutionModel -> SubstitutionModel
renameSubstitutionModel = set name

-- | Abbend to name.
appendNameSubstitutionModel :: SubstitutionModelName -> SubstitutionModel -> SubstitutionModel
appendNameSubstitutionModel n = over name (<> n)

-- | Summarize a substitution model; lines to be printed to screen or log.
summarizeSubstitutionModel :: SubstitutionModel -> [L.ByteString]
summarizeSubstitutionModel sm = map L.pack $
  (show (sm ^. code) ++ " substitution model: " ++ L.unpack (sm ^. name) ++ ".") :
  [ "Parameters: " ++ show (sm ^. params) ++ "." | not (null (sm ^. params))] ++
  case sm ^. code of
    DNA -> [ "Stationary distribution: " ++ show (sm ^. stationaryDistribution) ++ "."
           , "Exchangeability matrix: " ++ show (sm ^. exchangeabilityMatrix) ++ "." ]
    Protein -> [ "Stationary distribution: " ++ show (sm ^. stationaryDistribution) ++ "." ]
    _ -> []

-- | Calculate rate matrix from substitution model.
getRateMatrix :: SubstitutionModel -> RateMatrix
getRateMatrix sm = fromExchangeabilityMatrix (sm ^. exchangeabilityMatrix) (sm ^. stationaryDistribution)
