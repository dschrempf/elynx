-- |
-- Module      :  ELynx.Data.MarkovProcess.GammaRateHeterogeneity
-- Description :  Discrete gamma rate heterogeneity
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Feb 28 14:09:11 2019.
--
-- At the moment, a mixture model is used to emulate gamma rate heterogeneity. This
-- does not come with huge run time increases when simulating data. For inference
-- however, it would make a lot of sense to reuse the Eigendecomposition for all
-- rate heterogeneity components though.
module ELynx.Data.MarkovProcess.GammaRateHeterogeneity
  ( summarizeGammaRateHeterogeneity,
    expand,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import qualified ELynx.Data.MarkovProcess.MixtureModel as M
import qualified ELynx.Data.MarkovProcess.PhyloModel as P
import qualified ELynx.Data.MarkovProcess.SubstitutionModel as S
import Numeric.Integration.TanhSinh
import Statistics.Distribution
import Statistics.Distribution.Gamma
import Prelude hiding (repeat)

-- | Short summary of gamma rate heterogeneity parameters.
summarizeGammaRateHeterogeneity :: Int -> Double -> [BL.ByteString]
summarizeGammaRateHeterogeneity n alpha =
  map
    BL.pack
    [ "Discrete gamma rate heterogeneity.",
      "Number of categories: " ++ show n,
      "Shape parameter of gamma distribution: " ++ show alpha,
      "Rates: " ++ show (getMeans n alpha)
    ]

-- | For a given number of rate categories, a gamma shape parameter alpha and a
-- substitution model, compute the scaled substitution models corresponding to
-- the gamma rates.
expand :: Int -> Double -> P.PhyloModel -> P.PhyloModel
expand n alpha (P.SubstitutionModel sm) =
  P.MixtureModel $ expandSubstitutionModel n alpha sm
expand n alpha (P.MixtureModel mm) =
  P.MixtureModel $ expandMixtureModel n alpha mm

getName :: Int -> Double -> String
getName n alpha =
  " with discrete gamma rate heterogeneity; "
    ++ show n
    ++ " categories; "
    ++ "shape parameter "
    ++ show alpha

splitSubstitutionModel ::
  Int -> Double -> S.SubstitutionModel -> NonEmpty S.SubstitutionModel
splitSubstitutionModel n alpha sm = renamedSMs
  where
    means = getMeans n alpha
    scaledSMs = N.map (`S.scale` sm) means
    names = N.fromList $ map (("; gamma rate category " ++) . show) [1 :: Int ..]
    renamedSMs = N.zipWith S.appendName names scaledSMs

expandSubstitutionModel ::
  Int -> Double -> S.SubstitutionModel -> M.MixtureModel
expandSubstitutionModel n alpha sm = M.fromSubstitutionModels name ws sms
  where
    name = S.name sm <> getName n alpha
    ws = N.repeat 1.0
    sms = splitSubstitutionModel n alpha sm

expandMixtureModel :: Int -> Double -> M.MixtureModel -> M.MixtureModel
expandMixtureModel n alpha mm = M.concatenate name renamedMMs
  where
    name = M.name mm <> getName n alpha
    means = getMeans n alpha
    scaledMMs = N.map (`M.scale` mm) means
    names = N.fromList $ map (("; gamma rate category " ++) . show) [1 :: Int ..]
    renamedMMs = N.zipWith M.appendNameComponents names scaledMMs

-- For a given number of rate categories 'n' and a shape parameter 'alpha' (the
-- rate or scale is set such that the mean is 1.0), return a list of rates that
-- represent the respective categories. Use the mean rate for each category.
getMeans :: Int -> Double -> NonEmpty Double
getMeans n alpha = N.fromList $ means <> pure lastMean
  where
    gamma = gammaDistr alpha (1.0 / alpha)
    quantiles =
      [quantile gamma (fromIntegral i / fromIntegral n) | i <- [0 .. n]]
    -- Calculate the mean rate. Multiplication with the number of rate
    -- categories 'n' is necessary because in each n-quantile the
    -- probability mass is 1/n.
    meanFunc x = fromIntegral n * x * density gamma x
    -- Only calculate the first (n-1) categories with normal integration.
    means =
      [ integralAToB meanFunc (quantiles !! i) (quantiles !! (i + 1))
        | i <- [0 .. n - 2]
      ]
    -- The last category has to be calculated with an improper integration.
    lastMean = integralAToInf meanFunc (quantiles !! (n - 1))

-- The error of integration.
eps :: Double
eps = 1e-6

-- The integration method to use
method :: (Double -> Double) -> Double -> Double -> [Result]
method = parSimpson

-- Helper function for a normal integral from 'a' to 'b'.
integralAToB :: (Double -> Double) -> Double -> Double -> Double
integralAToB f a b = result . absolute eps $ method f a b

-- Helper function for an improper integral from 'a' to infinity.
integralAToInf :: (Double -> Double) -> Double -> Double
integralAToInf f a =
  (result . absolute eps $ nonNegative method f) - integralAToB f 0 a
