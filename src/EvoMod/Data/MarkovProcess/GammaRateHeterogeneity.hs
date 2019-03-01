{- |
Module      :  EvoMod.Data.MarkovProcess.GammaRateHeterogeneity
Description :  Discrete gamma rate heterogeneity
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 28 14:09:11 2019.

At the moment, a mixture model is used to emulate gamma rate heterogeneity. This
does not come with huge run time increases when simulating data. For inference
however, it would make a lot of sense to reuse the Eigendecomposition for all
rate heterogeneity components though.

-}

module EvoMod.Data.MarkovProcess.GammaRateHeterogeneity
  ( expand
  ) where

import qualified Data.ByteString.Lazy.Char8                  as L
import           Numeric.Integration.TanhSinh
import           Statistics.Distribution
import           Statistics.Distribution.Gamma

import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.PhyloModel
import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | For a given number of rate categories, a gamma shape parameter alpha and a
-- substitution model, compute the scaled substitution models corresponding to
-- the gamma rates.
expand :: Int -> Double -> PhyloModel -> PhyloModel
expand n alpha (PhyloSubstitutionModel sm)
  = PhyloMixtureModel $ expandSubstitutionModel n alpha sm
expand n alpha (PhyloMixtureModel mm)
  = PhyloMixtureModel $ expandMixtureModel n alpha mm

getName :: Int -> Double -> L.ByteString
getName n alpha = L.pack (" with discrete gamma rate heterogeneity; "
                           ++ show n ++ " categories; "
                           ++ "shape parameter " ++ show alpha)

splitSubstitutionModel :: Int -> Double -> SubstitutionModel -> [SubstitutionModel]
splitSubstitutionModel n alpha sm = renamedSMs
  where
    means = getMeans n alpha
    scaledSMs = map (scaleSubstitutionModel sm) means
    names = map (L.pack . ("; gamma rate category " ++) . show) [1 :: Int ..]
    renamedSMs = zipWith appendNameSubstitutionModel scaledSMs names

expandSubstitutionModel :: Int -> Double -> SubstitutionModel -> MixtureModel
expandSubstitutionModel n alpha sm = fromSubstitutionModels name (repeat 1.0) sms
  where
    name = smName sm <> getName n alpha
    sms  = splitSubstitutionModel n alpha sm

expandMixtureModel :: Int -> Double -> MixtureModel -> MixtureModel
expandMixtureModel n alpha mm = concatenateMixtureModels name renamedMMs
  where
    name = mmName mm <> getName n alpha
    means = getMeans n alpha
    scaledMMs = map (scaleMixtureModel mm) means
    names = map (L.pack . ("; gamma rate category " ++) . show) [1 :: Int ..]
    renamedMMs = zipWith appendNameMixtureModel scaledMMs names

-- For a given number of rate categories 'n' and a shape parameter 'alpha' (the
-- rate or scale is set such that the mean is 1.0), return a list of rates that
-- represent the respective categories. Use the mean rate for each category.
getMeans :: Int -> Double -> [Double]
getMeans n alpha = means ++ lastMean
  where gamma = gammaDistr alpha (1.0/alpha)
        quantiles = [ quantile gamma (fromIntegral i / fromIntegral n) | i <- [0..n] ]
        -- Calculate the mean rate. Multiplication with the number of rate
        -- categories 'n' is necessary because in each n-quantile the
        -- probability mass is 1/n.
        meanFunc x = fromIntegral n * x * density gamma x
        -- Only calculate the first (n-1) categories with normal integration.
        means = [ integralAToB meanFunc (quantiles !! i) (quantiles !! (i+1)) | i <- [0..n-2] ]
        -- The last category has to be calculated with an improper integration.
        lastMean = [integralAToInf meanFunc (quantiles !! (n-1))]

-- The error of integration.
eps :: Double
eps = 1e-6

-- The integration method to use
method :: (Double -> Double ) -> Double -> Double -> [Result]
method = parSimpson

-- Helper function for a normal integral from 'a' to 'b'.
integralAToB :: (Double -> Double) -> Double -> Double -> Double
integralAToB f a b = result . absolute eps $ method f a b

-- Helper function for an improper integral from 'a' to infinity.
integralAToInf :: (Double -> Double) -> Double -> Double
integralAToInf f a = (result . absolute eps $ nonNegative method f) - integralAToB f 0 a
