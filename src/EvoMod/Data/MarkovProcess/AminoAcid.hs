{- |
Module      :  EvoMod.Data.MarkovProcess.AminoAcid
Description :  Amino acid rate matrices such as LG
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 09:29:19 2019.

The order of amino acids is alphabetic.

-}

module EvoMod.Data.MarkovProcess.AminoAcid
  ( lg
  , lgCustom
  , poisson
  , poissonCustom
  ) where

import qualified Data.ByteString.Lazy.Char8                  as B
import           Data.List                                   (elemIndex)
import           Data.Maybe                                  (fromMaybe)
import           Data.Word                                   (Word8)
import           Numeric.LinearAlgebra
import           Numeric.SpecFunctions

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Tools.ByteString                     (c2w)
import           EvoMod.Tools.Vector
import           EvoMod.Tools.LinearAlgebra

-- XXX: Hardcoded here, to reduce intermodule dependencies.
n :: Int
n = 20

-- Some matrices have to be converted from PAML order to alphabetical order. See
-- 'pamlToAlphaVec' and 'pamlToAlphaMat'.

-- Amno acids in alphabetical order.
aaAlphaOrder :: [Word8]
aaAlphaOrder = map c2w [ 'A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 'M'
                       , 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'Y' ]

-- Amino acids in PAML oder.
aaPamlOrder :: [Word8]
aaPamlOrder = map c2w [ 'A', 'R', 'N', 'D', 'C', 'Q', 'E', 'G', 'H', 'I', 'L', 'K'
                      , 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V' ]

-- -- This is a very slow implementation; since I only convert matrices once it
-- -- should not be a problem. A map would be better if performance is an issue.
-- pamlIndexToAlphaIndex :: Int -> Int
-- pamlIndexToAlphaIndex i = fromMaybe
--                           (error $ "Could not convert index " ++ show i ++ ".")
--                           (elemIndex aa aaAlphaOrder)
--   where aa = aaPamlOrder !! i

-- This is a very slow implementation; since I only convert matrices once it
-- should not be a problem. A map would be better if performance is an issue.
alphaIndexToPamlIndex :: Int -> Int
alphaIndexToPamlIndex i = fromMaybe
                          (error $ "Could not convert index " ++ show i ++ ".")
                          (elemIndex aa aaPamlOrder)
  where aa = aaAlphaOrder !! i

-- Convert an amino acid vector in PAML order to a vector in alphabetical order.
pamlToAlphaVec :: Vector R -> Vector R
pamlToAlphaVec v = build n (\i -> v ! alphaIndexToPamlIndex (round i))


-- Convert an amino acid matrix in PAML order to a matrix in alphabetical order.
pamlToAlphaMat :: Matrix R -> Matrix R
pamlToAlphaMat m = build (n,n) (\i j -> m
                                 ! alphaIndexToPamlIndex (round i)
                                 ! alphaIndexToPamlIndex (round j))

-- Lower triangular matrix of LG exchangeabilities in PAML order and in form of
-- a list.
lgExchRawPaml :: [Double]
lgExchRawPaml = [0.425093, 0.276818, 0.751878, 0.395144, 0.123954, 5.076149,
                 2.489084, 0.534551, 0.528768, 0.062556, 0.969894, 2.807908,
                 1.695752, 0.523386, 0.084808, 1.038545, 0.363970, 0.541712,
                 5.243870, 0.003499, 4.128591, 2.066040, 0.390192, 1.437645,
                 0.844926, 0.569265, 0.267959, 0.348847, 0.358858, 2.426601,
                 4.509238, 0.927114, 0.640543, 4.813505, 0.423881, 0.311484,
                 0.149830, 0.126991, 0.191503, 0.010690, 0.320627, 0.072854,
                 0.044265, 0.008705, 0.108882, 0.395337, 0.301848, 0.068427,
                 0.015076, 0.594007, 0.582457, 0.069673, 0.044261, 0.366317,
                 4.145067, 0.536518, 6.326067, 2.145078, 0.282959, 0.013266,
                 3.234294, 1.807177, 0.296636, 0.697264, 0.159069, 0.137500,
                 1.124035, 0.484133, 0.371004, 0.025548, 0.893680, 1.672569,
                 0.173735, 0.139538, 0.442472, 4.273607, 6.312358, 0.656604,
                 0.253701, 0.052722, 0.089525, 0.017416, 1.105251, 0.035855,
                 0.018811, 0.089586, 0.682139, 1.112727, 2.592692, 0.023918,
                 1.798853, 1.177651, 0.332533, 0.161787, 0.394456, 0.075382,
                 0.624294, 0.419409, 0.196961, 0.508851, 0.078281, 0.249060,
                 0.390322, 0.099849, 0.094464, 4.727182, 0.858151, 4.008358,
                 1.240275, 2.784478, 1.223828, 0.611973, 1.739990, 0.990012,
                 0.064105, 0.182287, 0.748683, 0.346960, 0.361819, 1.338132,
                 2.139501, 0.578987, 2.000679, 0.425860, 1.143480, 1.080136,
                 0.604545, 0.129836, 0.584262, 1.033739, 0.302936, 1.136863,
                 2.020366, 0.165001, 0.571468, 6.472279, 0.180717, 0.593607,
                 0.045376, 0.029890, 0.670128, 0.236199, 0.077852, 0.268491,
                 0.597054, 0.111660, 0.619632, 0.049906, 0.696175, 2.457121,
                 0.095131, 0.248862, 0.140825, 0.218959, 0.314440, 0.612025,
                 0.135107, 1.165532, 0.257336, 0.120037, 0.054679, 5.306834,
                 0.232523, 0.299648, 0.131932, 0.481306, 7.803902, 0.089613,
                 0.400547, 0.245841, 3.151815, 2.547870, 0.170887, 0.083688,
                 0.037967, 1.959291, 0.210332, 0.245034, 0.076701, 0.119013,
                 10.649107, 1.702745, 0.185202, 1.898718, 0.654683, 0.296501,
                 0.098369, 2.188158, 0.189510, 0.249313]

-- Conversion from matrix indices (i,j) to list index k.
-- (i,j) k
--
-- (0,0) -
-- (1,0) 0  (1,1) -
-- (2,0) 1  (2,1) 2  (2,2) -
-- (3,0) 3  (3,1) 4  (3,2) 5 (3,3) -
-- (4,0) 6  (4,1) 7  (4,2) 8 (4,3) 9 (4,4) -
--
-- k = (i choose 2) + j.
ijToK :: Int -> Int -> Int
ijToK i j = round (i `choose` 2) + j

-- The function is a little weird because HMatrix uses Double indices for Matrix
-- Double builders.
lgExchPamlBuilder :: Double -> Double -> Double
lgExchPamlBuilder i j | i > j  = lgExchRawPaml !! ijToK iI jI
                      | i == j = 0.0
                      | i < j  = lgExchRawPaml !! ijToK jI iI
                      | otherwise = error "Float indices could not be compared during matrix creation."
  where iI = round i :: Int
        jI = round j :: Int

-- Exchangeability matrix of LG model in PAML order.
lgExchPaml :: ExchMatrix
lgExchPaml = build (n,n) lgExchPamlBuilder

-- | Exchangeabilities of LG model in alphabetical order.
lgExch :: ExchMatrix
lgExch = pamlToAlphaMat lgExchPaml

-- Stationary distribution in PAML order.
lgStatDistPaml :: StationaryDistribution
lgStatDistPaml = normalizeSumVec 1.0 $
  fromList [ 0.079066, 0.055941, 0.041977, 0.053052, 0.012937, 0.040767
           , 0.071586, 0.057337, 0.022355, 0.062157, 0.099081, 0.064600
           , 0.022951, 0.042302, 0.044040, 0.061197, 0.053287, 0.012066
           , 0.034155, 0.069147 ]

-- | Stationary distribution of LG model in alphabetical order.
lgStatDist :: StationaryDistribution
lgStatDist = pamlToAlphaVec lgStatDistPaml

-- | LG rate matrix with amino acids in alphabetical order.
lgRM :: RateMatrix
lgRM = lgRMCustom lgStatDist

-- | LG substitution model.
lg :: SubstitutionModel
lg = SubstitutionModel Protein (B.pack "LG") [] lgStatDist lgExch lgRM

-- | LG rate matrix with custom stationary distribution.
lgRMCustom :: StationaryDistribution -> RateMatrix
lgRMCustom = fromExchMatrix lgExch

-- | LG substitution model with custom stationary distribution and maybe a name.
lgCustom :: StationaryDistribution -> Maybe B.ByteString -> SubstitutionModel
lgCustom f mn = SubstitutionModel Protein name [] f lgExch (lgRMCustom f)
  where name = fromMaybe (B.pack "LG-Custom") mn

uniformExch :: ExchMatrix
uniformExch = matrixSetDiagToZero $ matrix n $ replicate (n*n) 1.0

poissonExch :: ExchMatrix
poissonExch = uniformExch

-- | Poisson rate matrix.
poissonRM :: RateMatrix
poissonRM = poissonRMCustom $ uniformVec n

-- | Poisson substitution model.
poisson :: SubstitutionModel
poisson = SubstitutionModel Protein (B.pack "Poisson") [] (uniformVec n) poissonExch poissonRM

-- | Poisson rate matrix with custom stationary distribution.
poissonRMCustom :: StationaryDistribution -> RateMatrix
poissonRMCustom = fromExchMatrix uniformExch

-- | Poisson substitution model with custom stationary distribution and maybe a name.
poissonCustom :: StationaryDistribution -> Maybe B.ByteString -> SubstitutionModel
poissonCustom f mn = SubstitutionModel Protein name [] f poissonExch (poissonRMCustom f)
  where name = fromMaybe (B.pack "Poisson-Custom") mn
