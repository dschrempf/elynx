{- |
Module      :  EvoMod.Data.MarkovProcess.CXXModels
Description :  C10 to C60 models
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Feb 26 16:44:33 2019.

Quang, L. S., Gascuel, O. & Lartillot, N. Empirical profile mixture models for
phylogenetic reconstruction. Bioinformatics 24, 2317–2323 (2008).

XXX: For now, I only provide Poisson exchangeabilities.

-}

module EvoMod.Data.MarkovProcess.CXXModels
  ( cxx
  ) where

import qualified Data.ByteString.Builder                     as B
import qualified Data.ByteString.Lazy.Char8                  as B

import           EvoMod.Data.MarkovProcess.AminoAcid
import           EvoMod.Data.MarkovProcess.CXXModelsData
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel

cxx :: Int -> MixtureModel
cxx 10 = c10
cxx 20 = c20
cxx 30 = c30
cxx 40 = c40
cxx 50 = c50
cxx 60 = c60
cxx _  = error "CXX models only available with 10, 20, 30, 40, 50, and 60 components."

c10 :: MixtureModel
c10 = cxxFromStatDistsAndWeights c10Weights c10StatDists

c20 :: MixtureModel
c20 = cxxFromStatDistsAndWeights c20Weights c20StatDists

c30 :: MixtureModel
c30 = cxxFromStatDistsAndWeights c30Weights c30StatDists

c40 :: MixtureModel
c40 = cxxFromStatDistsAndWeights c40Weights c40StatDists

c50 :: MixtureModel
c50 = cxxFromStatDistsAndWeights c50Weights c50StatDists

c60 :: MixtureModel
c60 = cxxFromStatDistsAndWeights c60Weights c60StatDists

cxxName :: Int -> B.Builder
cxxName nComps = B.char8 'C' <> B.intDec nComps

componentName :: Int -> Int -> B.ByteString
componentName nComps comp = B.toLazyByteString $
  cxxName nComps <> B.string8 "Comp" <> B.intDec comp

cxxSubstitutionModelFromStatDist :: Int -> Int -> StationaryDistribution -> SubstitutionModel
cxxSubstitutionModelFromStatDist nComps comp d = poissonCustom d (Just name)
  where name = componentName nComps comp

cxxSubstitutionModelsFromStatDists :: [StationaryDistribution] -> [SubstitutionModel]
cxxSubstitutionModelsFromStatDists ds = zipWith (cxxSubstitutionModelFromStatDist nComp) [1..] ds
  where nComp = length ds

cxxFromStatDistsAndWeights :: [Weight] -> [StationaryDistribution] -> MixtureModel
cxxFromStatDistsAndWeights ws ds = MixtureModel (B.toLazyByteString $ cxxName nComps) comps
  where
    nComps = length ds
    comps = zipWith MixtureModelComponent ws (cxxSubstitutionModelsFromStatDists ds)
