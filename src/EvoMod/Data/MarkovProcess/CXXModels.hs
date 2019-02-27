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
  ( c10
  , c10CustomWeights
  , c20
  , c20CustomWeights
  , c30
  , c30CustomWeights
  , c40
  , c40CustomWeights
  , c50
  , c50CustomWeights
  , c60
  , c60CustomWeights
  ) where

import qualified Data.ByteString.Builder                     as B
import qualified Data.ByteString.Lazy.Char8                  as B

import           EvoMod.Data.MarkovProcess.AminoAcid
import           EvoMod.Data.MarkovProcess.CXXModelsData
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel

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

c10CustomWeights :: Maybe [Weight] -> MixtureModel
c10CustomWeights (Just ws)
  | length ws == 10 = cxxFromStatDistsAndWeights ws c10StatDists
  | otherwise       = error "Number of weights does not match C60 model."
c10CustomWeights Nothing = c10

c20CustomWeights :: Maybe [Weight] -> MixtureModel
c20CustomWeights (Just ws)
  | length ws == 20 = cxxFromStatDistsAndWeights ws c20StatDists
  | otherwise       = error "Number of weights does not match C60 model."
c20CustomWeights Nothing = c20

c30CustomWeights :: Maybe [Weight] -> MixtureModel
c30CustomWeights (Just ws)
  | length ws == 30 = cxxFromStatDistsAndWeights ws c30StatDists
  | otherwise       = error "Number of weights does not match C60 model."
c30CustomWeights Nothing = c30

c40CustomWeights :: Maybe [Weight] -> MixtureModel
c40CustomWeights (Just ws)
  | length ws == 40 = cxxFromStatDistsAndWeights ws c40StatDists
  | otherwise       = error "Number of weights does not match C60 model."
c40CustomWeights Nothing = c40

c50CustomWeights :: Maybe [Weight] -> MixtureModel
c50CustomWeights (Just ws)
  | length ws == 50 = cxxFromStatDistsAndWeights ws c50StatDists
  | otherwise       = error "Number of weights does not match C60 model."
c50CustomWeights Nothing = c50

c60CustomWeights :: Maybe [Weight] -> MixtureModel
c60CustomWeights (Just ws)
  | length ws == 60 = cxxFromStatDistsAndWeights ws c60StatDists
  | otherwise       = error "Number of weights does not match C60 model."
c60CustomWeights Nothing = c60

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
