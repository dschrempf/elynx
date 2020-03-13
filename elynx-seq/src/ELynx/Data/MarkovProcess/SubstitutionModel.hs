{- |
Module      :  ELynx.Data.MarkovProcess.SubstitutionModel
Description :  Data type describing substitution model
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 19:10:46 2019.

To be imported qualified.

-}

module ELynx.Data.MarkovProcess.SubstitutionModel
  ( -- * Types
    Name
  , Params
  , SubstitutionModel
  -- * Lenses and other accessors
  , alphabet
  , name
  , stationaryDistribution
  , exchangeabilityMatrix
  , rateMatrix
  , totalRate
  -- * Building substitution models
  , substitutionModel
  , unnormalized
  -- * Transformations
  , scale
  , normalize
  , appendName
  -- * Output
  , summarize
  )
where

import qualified Data.ByteString.Lazy.Char8    as L
import qualified Numeric.LinearAlgebra         as LinAlg

import           ELynx.Data.Alphabet.Alphabet
import qualified ELynx.Data.MarkovProcess.RateMatrix
                                               as R
import           ELynx.Tools.Definitions
import           ELynx.Tools.LinearAlgebra
import           ELynx.Tools.Numeric

-- | Name of substitution model; abstracted and subject to change.
type Name = String

-- | Parameters of substitution model. May be the empty list.
type Params = [Double]

-- | Complete definition of a substitution model. Create instances with
-- 'substitutionModel'. A substitution model has an alphabet, a name, and a list
-- of parameters (e.g., the kappa value for the HKY model). Further, the
-- transition rate matrix is defined by a stationary distribution and a set of
-- exchangeabilities.
data SubstitutionModel = SubstitutionModel
  { alphabet               :: Alphabet -- ^ Alphabet
  , name                   :: Name     -- ^ Name
  , params                 :: Params   -- ^ List of parameters
  , stationaryDistribution :: R.StationaryDistribution -- ^ Stationary distribution
  , exchangeabilityMatrix  :: R.ExchangeabilityMatrix  -- ^ Exchangeability matrix
  }
  deriving (Show, Read)

-- | Calculate rate matrix from substitution model.
rateMatrix :: SubstitutionModel -> R.RateMatrix
rateMatrix sm = R.fromExchangeabilityMatrix (exchangeabilityMatrix sm)
                                            (stationaryDistribution sm)

-- | Get scale of substitution model.
totalRate :: SubstitutionModel -> Double
totalRate sm = R.totalRate (rateMatrix sm)

-- | Create normalized 'SubstitutionModel'. See 'normalize'.
substitutionModel
  :: Alphabet
  -> Name
  -> Params
  -> R.StationaryDistribution
  -> R.ExchangeabilityMatrix
  -> SubstitutionModel
substitutionModel c n ps d e = if R.isValid d
  then normalize $ SubstitutionModel c n ps d e
  else error "Stationary distribution does not sum to 1.0."

-- | Create UNNORMALIZED 'SubstitutionModel'. See 'substitutionModel'.
unnormalized
  :: Alphabet
  -> Name
  -> Params
  -> R.StationaryDistribution
  -> R.ExchangeabilityMatrix
  -> SubstitutionModel
unnormalized = SubstitutionModel

-- | Scale the rate of a substitution model by given factor.
scale :: Double -> SubstitutionModel -> SubstitutionModel
scale r sm = sm { exchangeabilityMatrix = em' }
  where em' = LinAlg.scale r $ exchangeabilityMatrix sm

-- | Normalize a substitution model, so that, on average, one substitution
-- happens per unit time.
normalize :: SubstitutionModel -> SubstitutionModel
normalize sm = scale (1.0 / r) sm where r = totalRate sm

-- | Abbend to name.
appendName :: Name -> SubstitutionModel -> SubstitutionModel
appendName n sm = sm { name = n' } where n' = name sm <> n

-- | Summarize a substitution model; lines to be printed to screen or log.
summarize :: SubstitutionModel -> [L.ByteString]
summarize sm =
  map L.pack
    $  (show (alphabet sm) ++ " substitution model: " ++ name sm ++ ".")
    :  [ "Parameters: " ++ show (params sm) ++ "." | not (null (params sm)) ]
    ++ case alphabet sm of
         DNA ->
           [ "Stationary distribution: "
             ++ dispv precision (stationaryDistribution sm)
             ++ "."
           , "Exchangeability matrix:\n"
             ++ dispmi 2 precision (exchangeabilityMatrix sm)
             ++ "."
           , "Scale: " ++ show (roundN precision $ totalRate sm) ++ "."
           ]
         Protein ->
           [ "Stationary distribution: "
             ++ dispv precision (stationaryDistribution sm)
             ++ "."
           , "Scale: " ++ show (roundN precision $ totalRate sm) ++ "."
           ]
         _ ->
           error
             "Extended character sets are not supported with substitution models."

