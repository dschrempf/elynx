{- |
Module      :  EvoMod.Data.RateMatrix.Nucleotide
Description :  Substitution models using nucleotides
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 24 08:33:26 2019.

XXX: Maybe rename to something like /DNA substitution models/. Nucleotide ~
Alphabet; DNA ~ Character.

The order of nucleotides is A, C, G, T; see 'EvoMod.Data.Alphabet.Nucleotide'.

-}

module EvoMod.Data.RateMatrix.Nucleotide
  ( jc
  , hky
  ) where

import           Numeric.LinearAlgebra             hiding (normalize)

import           EvoMod.Data.RateMatrix.RateMatrix

n :: Int
-- n = cardinality (alphabet DNA)
-- XXX: Hardcoded here, to reduce intermodule dependencies.
n = 4

-- Improve safety by constructing matrices from a set of rates between
-- 'Nucleotide's.
-- jcFromNuc :: Nucleotide -> Nucleotide -> Double
-- jcFromNuc i j | i == j    = 0.0
--               | otherwise = 1.0

-- | JC model.
jc :: RateMatrix
jc = normalize $ setDiagonal $ (n><n)
     [ 0.0, 1.0, 1.0, 1.0
     , 1.0, 0.0, 1.0, 1.0
     , 1.0, 1.0, 0.0, 1.0
     , 1.0, 1.0, 1.0, 0.0 ]

-- | HKY model with kappa.
hky :: Double -> StationaryDist -> RateMatrix
hky k = fromExchMatrix em
  where em = (n><n)
             [ 0.0, 1.0,   k, 1.0
             , 1.0, 0.0, 1.0,   k
             ,   k, 1.0, 0.0, 1.0
             , 1.0,   k, 1.0, 0.0 ]

