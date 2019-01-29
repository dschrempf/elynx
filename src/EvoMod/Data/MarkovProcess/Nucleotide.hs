{- |
Module      :  EvoMod.Data.MarkovProcess.Nucleotide
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

module EvoMod.Data.MarkovProcess.Nucleotide
  ( jc
  , jcModel
  , hky
  , hkyModel
  ) where

import           Numeric.LinearAlgebra                       hiding (normalize)

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Tools                                (uniformVec)

n :: Int
n = cardinality (alphabet DNA)

-- | JC model matrix.
jc :: RateMatrix
jc = normalize $ setDiagonal $
  (n><n)
  [ 0.0, 1.0, 1.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 1.0, 1.0, 0.0, 1.0
  , 1.0, 1.0, 1.0, 0.0 ]

-- | JC substitution model.
jcModel :: SubstitutionModel
jcModel = SubstitutionModel DNA "JC" [] f e jc
  where f = uniformVec n
        e = toExchMatrix jc f

hkyExchMatrix :: Double -> ExchMatrix
hkyExchMatrix k =
  (n><n)
  [ 0.0, 1.0,   k, 1.0
  , 1.0, 0.0, 1.0,   k
  ,   k, 1.0, 0.0, 1.0
  , 1.0,   k, 1.0, 0.0 ]

-- | HKY matrix with kappa and stationary distribution.
hky :: Double -> StationaryDistribution -> RateMatrix
hky = fromExchMatrix . hkyExchMatrix

-- | HKY substitution model.
hkyModel :: Double -> StationaryDistribution -> SubstitutionModel
hkyModel k f = SubstitutionModel DNA "HKY" [k] f e m
  where e = hkyExchMatrix k
        m = hky k f
