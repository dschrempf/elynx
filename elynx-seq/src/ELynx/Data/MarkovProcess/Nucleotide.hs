{- |
Module      :  ELynx.Data.MarkovProcess.Nucleotide
Description :  Substitution models using nucleotides
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 24 08:33:26 2019.

XXX: Maybe rename to something like /DNA substitution models/. Nucleotide ~
Alphabet; DNA ~ Character.

The order of nucleotides is A, C, G, T; see 'ELynx.Data.Character.Nucleotide'.

-}

module ELynx.Data.MarkovProcess.Nucleotide
  ( jc
  , hky
  ) where

import           Numeric.LinearAlgebra                      hiding (normalize)

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Data.MarkovProcess.RateMatrix
import           ELynx.Data.MarkovProcess.SubstitutionModel
import           ELynx.Tools.Vector

-- XXX: Another idea of structuring the code. This would probably be cleaner in
-- the long run.

-- data PhyloModel = MixtureModel | SubstitutionModel

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
  (n><n)
  [ 0.0, 1.0, 1.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 1.0, 1.0, 0.0, 1.0
  , 1.0, 1.0, 1.0, 0.0 ]

-- | JC substitution model.
jc :: SubstitutionModel
jc = substitutionModel DNA "JC" [] f jcExch
  where f = uniformVec n

hkyExch :: Double -> ExchangeabilityMatrix
hkyExch k =
  (n><n)
  [ 0.0, 1.0,   k, 1.0
  , 1.0, 0.0, 1.0,   k
  ,   k, 1.0, 0.0, 1.0
  , 1.0,   k, 1.0, 0.0 ]

-- | HKY substitution model.
hky :: Double -> StationaryDistribution -> SubstitutionModel
hky k f = substitutionModel DNA "HKY" [k] f e
  where e = hkyExch k
