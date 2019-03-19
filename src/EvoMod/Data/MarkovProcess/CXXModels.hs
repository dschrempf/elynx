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
  (
    cxx
  ) where

import qualified Data.ByteString.Builder                     as L
import qualified Data.ByteString.Lazy.Char8                  as L

import           EvoMod.Data.MarkovProcess.AminoAcid
import           EvoMod.Data.MarkovProcess.CXXModelsData
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | Create CXX model with given number of components and probably with custom
-- weights.
cxx :: Int -> Maybe [Weight] -> Maybe MixtureModel
cxx 10 (Just ws) = Just $ c10CustomWeights ws
cxx 20 (Just ws) = Just $ c20CustomWeights ws
cxx 30 (Just ws) = Just $ c30CustomWeights ws
cxx 40 (Just ws) = Just $ c40CustomWeights ws
cxx 50 (Just ws) = Just $ c50CustomWeights ws
cxx 60 (Just ws) = Just $ c60CustomWeights ws
cxx 10 Nothing   = Just c10
cxx 20 Nothing   = Just c20
cxx 30 Nothing   = Just c30
cxx 40 Nothing   = Just c40
cxx 50 Nothing   = Just c50
cxx 60 Nothing   = Just c60
cxx _ _          = Nothing

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
c10CustomWeights :: [Weight] -> MixtureModel
c10CustomWeights ws
  | length ws == 10 = cxxFromStatDistsAndWeights ws c10StatDists
  | otherwise       = error "Number of weights does not match C10 model."

-- | C20 model with custom weights.
c20CustomWeights :: [Weight] -> MixtureModel
c20CustomWeights ws
  | length ws == 20 = cxxFromStatDistsAndWeights ws c20StatDists
  | otherwise       = error "Number of weights does not match C20 model."

-- | C30 model with custom weights.
c30CustomWeights :: [Weight] -> MixtureModel
c30CustomWeights ws
  | length ws == 30 = cxxFromStatDistsAndWeights ws c30StatDists
  | otherwise       = error "Number of weights does not match C30 model."

-- | C40 model with custom weights.
c40CustomWeights :: [Weight] -> MixtureModel
c40CustomWeights ws
  | length ws == 40 = cxxFromStatDistsAndWeights ws c40StatDists
  | otherwise       = error "Number of weights does not match C40 model."

-- | C50 model with custom weights.
c50CustomWeights :: [Weight] -> MixtureModel
c50CustomWeights ws
  | length ws == 50 = cxxFromStatDistsAndWeights ws c50StatDists
  | otherwise       = error "Number of weights does not match C50 model."

-- | C60 model with custom weights.
c60CustomWeights :: [Weight] -> MixtureModel
c60CustomWeights ws
  | length ws == 60 = cxxFromStatDistsAndWeights ws c60StatDists
  | otherwise       = error "Number of weights does not match C60 model."

cxxName :: Int -> L.Builder
cxxName nComps = L.char8 'C' <> L.intDec nComps

componentName :: Int -> Int -> L.ByteString
componentName nComps comp = L.toLazyByteString $
  cxxName nComps <> L.string8 "; component " <> L.intDec comp

cxxSubstitutionModelFromStatDist :: Int -> Int -> StationaryDistribution -> SubstitutionModel
-- XXX: Is it necessary (or even correct) to normalize the substitution model
-- here? For Poisson exchangeabilities, it doesn't make any difference, but for
-- other exchangeabilities, it will make a difference. How is this implemented
-- in other software packages?
cxxSubstitutionModelFromStatDist nComps comp d = normalizeSubstitutionModel $
                                                 poissonCustom (Just name) d
  where name = componentName nComps comp

cxxSubstitutionModelsFromStatDists :: [StationaryDistribution] -> [SubstitutionModel]
cxxSubstitutionModelsFromStatDists ds = zipWith (cxxSubstitutionModelFromStatDist nComp) [1..] ds
  where nComp = length ds

cxxFromStatDistsAndWeights :: [Weight] -> [StationaryDistribution] -> MixtureModel
cxxFromStatDistsAndWeights ws ds = MixtureModel (L.toLazyByteString $ cxxName nComps) comps
  where
    nComps = length ds
    comps = zipWith MixtureModelComponent ws (cxxSubstitutionModelsFromStatDists ds)
