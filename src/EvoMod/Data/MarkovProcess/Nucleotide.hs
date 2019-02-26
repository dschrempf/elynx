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
  , hky
  ) where

import qualified Data.ByteString.Lazy.Char8                  as B

import           Numeric.LinearAlgebra                       hiding (normalize)

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Tools.Vector

-- XXX: Another idea of structuring the code. This would probably be cleaner in
-- the long run.

-- data PhyloModel = MixtureModel | SubstitutionModel

-- data MixtureModel = [(Weight, SubstitutionModel)]

-- data SubstitutionModel = SMDNA DNASubstitutionModel | SMAA AASubstitutionModel

-- data DNASubstitutionModel = JC | HKY Double StationaryDistribution

-- data AASubstitutionModel = LG | ...

n :: Int
n = cardinality (alphabet DNA)

-- | JC model matrix.
rmJC :: RateMatrix
rmJC = normalize $ setDiagonal $
  (n><n)
  [ 0.0, 1.0, 1.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 1.0, 1.0, 0.0, 1.0
  , 1.0, 1.0, 1.0, 0.0 ]

-- | JC substitution model.
jc :: SubstitutionModel
jc = SubstitutionModel DNA (B.pack "JC") [] f e rmJC
  where f = uniformVec n
        e = toExchMatrix rmJC f

exchHKY :: Double -> ExchMatrix
exchHKY k =
  (n><n)
  [ 0.0, 1.0,   k, 1.0
  , 1.0, 0.0, 1.0,   k
  ,   k, 1.0, 0.0, 1.0
  , 1.0,   k, 1.0, 0.0 ]

-- | HKY matrix with kappa and stationary distribution.
rmHKY :: Double -> StationaryDistribution -> RateMatrix
rmHKY = fromExchMatrix . exchHKY

-- | HKY substitution model.
hky :: Double -> StationaryDistribution -> SubstitutionModel
hky k f = SubstitutionModel DNA (B.pack "HKY") [k] f e m
  where e = exchHKY k
        m = rmHKY k f
