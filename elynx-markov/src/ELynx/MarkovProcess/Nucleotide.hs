-- |
-- Module      :  ELynx.MarkovProcess.Nucleotide
-- Description :  Substitution models using nucleotides
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 24 08:33:26 2019.
--
-- XXX: Maybe rename to something like /DNA substitution models/. Nucleotide ~
-- Alphabet; DNA ~ Character.
--
-- The order of nucleotides is A, C, G, T; see 'ELynx.Character.Nucleotide'.
--
-- For the different DNA substitution models, please see
-- https://en.wikipedia.org/wiki/Models_of_DNA_evolution
module ELynx.MarkovProcess.Nucleotide
  ( jc,
    f81,
    hky,
    gtr4,
  )
where

import qualified Data.Vector.Storable as V
import ELynx.Alphabet.Alphabet
import ELynx.MarkovProcess.RateMatrix
import ELynx.MarkovProcess.SubstitutionModel
import Numeric.LinearAlgebra hiding (normalize)

-- XXX: Another idea of structuring the code. This would probably be cleaner in
-- the long run.

-- data PhyloModel = MixtureModel | SubstitutionModel

--
-- I think it could simply be:
-- data PhyloModel = [(Weight, SubstitutionModel)]
--

-- data MixtureModel = [(Weight, SubstitutionModel)]

-- data SubstitutionModel = SMDNA DNASubstitutionModel | SMAA AASubstitutionModel

-- data DNASubstitutionModel = JC | HKY Double StationaryDistribution

-- data AASubstitutionModel = LG | ...

n :: Int
-- n = length (alphabet :: [Nucleotide])
-- Hard code this here. Reduces model dependencies, and number of nucleotides
-- will not change.
n = 4

-- | JC model matrix.
jcExch :: ExchangeabilityMatrix
jcExch =
  (n >< n)
    [ 0.0,
      1.0,
      1.0,
      1.0,
      1.0,
      0.0,
      1.0,
      1.0,
      1.0,
      1.0,
      0.0,
      1.0,
      1.0,
      1.0,
      1.0,
      0.0
    ]

uniformVec :: Vector Double
uniformVec = V.replicate n (1 / fromIntegral n)

-- | JC substitution model.
jc :: SubstitutionModel
jc = substitutionModel DNA "JC" [] uniformVec jcExch

-- | F81 substitution model.
f81 :: StationaryDistribution -> SubstitutionModel
f81 d = substitutionModel DNA "F81" [] d jcExch

hkyExch :: Double -> ExchangeabilityMatrix
hkyExch k =
  (n >< n)
    [0.0, 1.0, k, 1.0, 1.0, 0.0, 1.0, k, k, 1.0, 0.0, 1.0, 1.0, k, 1.0, 0.0]

-- | HKY substitution model.
hky :: Double -> StationaryDistribution -> SubstitutionModel
hky k d = substitutionModel DNA "HKY" [k] d e where e = hkyExch k

-- | HKY substitution model.
gtr4 :: [Double] -> StationaryDistribution -> SubstitutionModel
gtr4 es d = substitutionModel DNA "GTR" es d e
  where
    e = exchFromListUpper n es
