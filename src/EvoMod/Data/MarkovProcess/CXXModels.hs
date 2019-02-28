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

import qualified Data.ByteString.Builder                     as L
import qualified Data.ByteString.Lazy.Char8                  as L

import           EvoMod.Data.MarkovProcess.AminoAcid
import           EvoMod.Data.MarkovProcess.CXXModelsData
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | C10 model.
c10 :: MixtureModel
c10 = cxxFromStatDistsAndWeights c10Weights c10StatDists

-- | C20 model.
c20 :: MixtureModel
c20 = cxxFromStatDistsAndWeights c20Weights c20StatDists

-- | C30 model.
c30 :: MixtureModel
c30 = cxxFromStatDistsAndWeights c30Weights c30StatDists

-- | C40 model.
c40 :: MixtureModel
c40 = cxxFromStatDistsAndWeights c40Weights c40StatDists

-- | C50 model.
c50 :: MixtureModel
c50 = cxxFromStatDistsAndWeights c50Weights c50StatDists

-- | C60 model.
c60 :: MixtureModel
c60 = cxxFromStatDistsAndWeights c60Weights c60StatDists

-- | C10 model with custom weights.
c10CustomWeights :: Maybe [Weight] -> MixtureModel
c10CustomWeights (Just ws)
  | length ws == 10 = cxxFromStatDistsAndWeights ws c10StatDists
  | otherwise       = error "Number of weights does not match C10 model."
c10CustomWeights Nothing = c10

-- | C20 model with custom weights.
c20CustomWeights :: Maybe [Weight] -> MixtureModel
c20CustomWeights (Just ws)
  | length ws == 20 = cxxFromStatDistsAndWeights ws c20StatDists
  | otherwise       = error "Number of weights does not match C20 model."
c20CustomWeights Nothing = c20

-- | C30 model with custom weights.
c30CustomWeights :: Maybe [Weight] -> MixtureModel
c30CustomWeights (Just ws)
  | length ws == 30 = cxxFromStatDistsAndWeights ws c30StatDists
  | otherwise       = error "Number of weights does not match C30 model."
c30CustomWeights Nothing = c30

-- | C40 model with custom weights.
c40CustomWeights :: Maybe [Weight] -> MixtureModel
c40CustomWeights (Just ws)
  | length ws == 40 = cxxFromStatDistsAndWeights ws c40StatDists
  | otherwise       = error "Number of weights does not match C40 model."
c40CustomWeights Nothing = c40

-- | C50 model with custom weights.
c50CustomWeights :: Maybe [Weight] -> MixtureModel
c50CustomWeights (Just ws)
  | length ws == 50 = cxxFromStatDistsAndWeights ws c50StatDists
  | otherwise       = error "Number of weights does not match C50 model."
c50CustomWeights Nothing = c50

-- | C60 model with custom weights.
c60CustomWeights :: Maybe [Weight] -> MixtureModel
c60CustomWeights (Just ws)
  | length ws == 60 = cxxFromStatDistsAndWeights ws c60StatDists
  | otherwise       = error "Number of weights does not match C60 model."
c60CustomWeights Nothing = c60

cxxName :: Int -> L.Builder
cxxName nComps = L.char8 'C' <> L.intDec nComps

componentName :: Int -> Int -> L.ByteString
componentName nComps comp = L.toLazyByteString $
  cxxName nComps <> L.string8 "Comp" <> L.intDec comp

cxxSubstitutionModelFromStatDist :: Int -> Int -> StationaryDistribution -> SubstitutionModel
cxxSubstitutionModelFromStatDist nComps comp d = poissonCustom d (Just name)
  where name = componentName nComps comp

cxxSubstitutionModelsFromStatDists :: [StationaryDistribution] -> [SubstitutionModel]
cxxSubstitutionModelsFromStatDists ds = zipWith (cxxSubstitutionModelFromStatDist nComp) [1..] ds
  where nComp = length ds

cxxFromStatDistsAndWeights :: [Weight] -> [StationaryDistribution] -> MixtureModel
cxxFromStatDistsAndWeights ws ds = MixtureModel (L.toLazyByteString $ cxxName nComps) comps
  where
    nComps = length ds
    comps = zipWith MixtureModelComponent ws (cxxSubstitutionModelsFromStatDists ds)
