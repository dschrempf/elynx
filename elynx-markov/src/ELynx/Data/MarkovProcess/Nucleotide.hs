{- |
Module      :  ELynx.Data.MarkovProcess.Nucleotide
Description :  Substitution models using nucleotides
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 24 08:33:26 2019.

XXX: Maybe rename to something like /DNA substitution models/. Nucleotide ~
Alphabet; DNA ~ Character.

The order of nucleotides is A, C, G, T; see 'ELynx.Data.Character.Nucleotide'.

For the different DNA substitution models, please see
https://en.wikipedia.org/wiki/Models_of_DNA_evolution

-}

module ELynx.Data.MarkovProcess.Nucleotide
  ( jc
  , f81
  , hky
  , gtr4
  )
where

import           Numeric.LinearAlgebra   hiding ( normalize )

import           ELynx.Data.Alphabet.Alphabet

import           ELynx.Tools

import           ELynx.Data.MarkovProcess.RateMatrix
import           ELynx.Data.MarkovProcess.SubstitutionModel

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
jcExch = (n >< n)
  [ 0.0
  , 1.0
  , 1.0
  , 1.0
  , 1.0
  , 0.0
  , 1.0
  , 1.0
  , 1.0
  , 1.0
  , 0.0
  , 1.0
  , 1.0
  , 1.0
  , 1.0
  , 0.0
  ]

-- | JC substitution model.
jc :: SubstitutionModel
jc = substitutionModel DNA "JC" [] d jcExch where d = uniformVec n

-- | F81 substitution model.
f81 :: StationaryDistribution -> SubstitutionModel
f81 d = substitutionModel DNA "F81" [] d jcExch

hkyExch :: Double -> ExchangeabilityMatrix
hkyExch k = (n >< n)
  [0.0, 1.0, k, 1.0, 1.0, 0.0, 1.0, k, k, 1.0, 0.0, 1.0, 1.0, k, 1.0, 0.0]

-- | HKY substitution model.
hky :: Double -> StationaryDistribution -> SubstitutionModel
hky k d = substitutionModel DNA "HKY" [k] d e where e = hkyExch k

-- | HKY substitution model.
gtr4 :: [Double] -> StationaryDistribution -> SubstitutionModel
gtr4 es d = substitutionModel DNA "GTR" es d e
  where e = exchFromListUpper n es
