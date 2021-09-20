{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  SLynx.Simulate.PhyloModel
-- Description :  Parse and interpret the model string
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Feb  1 13:32:16 2019.
module SLynx.Simulate.PhyloModel
  ( getPhyloModel,
  )
where

import Control.Applicative
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Either (rights)
import Data.Maybe
import qualified Data.Vector as V
import ELynx.Import.MarkovProcess.EDMModelPhylobayes
  ( EDMComponent,
  )
import ELynx.MarkovProcess.AminoAcid
import ELynx.MarkovProcess.CXXModels
import qualified ELynx.MarkovProcess.MixtureModel as M
import ELynx.MarkovProcess.Nucleotide
import qualified ELynx.MarkovProcess.PhyloModel as P
import ELynx.MarkovProcess.RateMatrix
import qualified ELynx.MarkovProcess.SubstitutionModel as S
import ELynx.Tools.Equality
import ELynx.Tools.InputOutput
import Numeric.LinearAlgebra
  ( norm_1,
    size,
    vector,
  )

nNuc :: Int
-- nNuc = length (alphabet :: [Nucleotide])
nNuc = 4

nAA :: Int
-- nAA = length (alphabet :: [AminoAcid])
nAA = 20

-- Model parameters between square brackets.
paramsStart :: Char
paramsStart = '['

paramsEnd :: Char
paramsEnd = ']'

-- Stationary distribution between curly brackets.
sdStart :: Char
sdStart = '{'

sdEnd :: Char
sdEnd = '}'

-- Mixture model components between round brackets.
mmStart :: Char
mmStart = '('

mmEnd :: Char
mmEnd = ')'

separator :: Char
separator = ','

name :: Parser String
name =
  BS.unpack
    <$> takeWhile1 (notInClass [paramsStart, paramsEnd, sdStart, sdEnd, mmStart, mmEnd, separator])

params :: Parser [Double]
params = char paramsStart *> double `sepBy1` char separator <* char paramsEnd

stationaryDistribution :: Parser StationaryDistribution
stationaryDistribution = do
  _ <- char sdStart
  f <- vector <$> double `sepBy1` char separator
  _ <- char sdEnd
  if nearlyEq (norm_1 f) 1.0
    then return f
    else
      error $
        "Sum of stationary distribution is "
          ++ show (norm_1 f)
          ++ " but should be 1.0."

assertLength :: StationaryDistribution -> Int -> a -> a
assertLength d n r =
  if size d /= n
    then
      error $
        "Length of stationary distribution is "
          ++ show (size d)
          ++ " but should be "
          ++ show n
          ++ "."
    else r

-- This is the main function that connects the model string, the parameters and
-- the stationary distribution. It should check that the model is valid.
assembleSubstitutionModel ::
  String ->
  Maybe S.Params ->
  Maybe StationaryDistribution ->
  Either String S.SubstitutionModel
-- DNA models.
assembleSubstitutionModel "JC" Nothing Nothing = Right jc
assembleSubstitutionModel "F81" Nothing (Just d) =
  Right $ assertLength d nNuc $ f81 d
assembleSubstitutionModel "HKY" (Just [k]) (Just d) =
  Right $ assertLength d nNuc $ hky k d
assembleSubstitutionModel "GTR4" (Just es) (Just d) =
  Right $ assertLength d nNuc $ gtr4 es d
-- Protein models.
assembleSubstitutionModel "Poisson" Nothing Nothing = Right poisson
assembleSubstitutionModel "Poisson-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ poissonCustom Nothing d
assembleSubstitutionModel "LG" Nothing Nothing = Right lg
assembleSubstitutionModel "LG-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ lgCustom Nothing d
assembleSubstitutionModel "WAG" Nothing Nothing = Right wag
assembleSubstitutionModel "WAG-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ wagCustom Nothing d
assembleSubstitutionModel "GTR20" (Just es) (Just d) =
  Right $ assertLength d nAA $ gtr20 es d
-- Ohterwisse, we cannot assemble the model.
assembleSubstitutionModel n mps mf =
  Left $
    unlines
      [ "Cannot assemble substitution model.",
        "Name: " ++ show n,
        "Parameters: " ++ show mps,
        "Stationary distribution: " ++ show mf
      ]

parseSubstitutionModel :: Parser S.SubstitutionModel
parseSubstitutionModel = do
  n <- name
  mps <- optional params
  mf <- optional stationaryDistribution
  let esm = assembleSubstitutionModel n mps mf
  case esm of
    Left err -> fail err
    Right sm -> return sm

edmModel :: [EDMComponent] -> Maybe [M.Weight] -> Parser M.MixtureModel
edmModel cs mws = do
  _ <- string "EDM"
  _ <- char mmStart
  n <- name
  mps <- optional params
  _ <- char mmEnd
  when (null cs) $ error "edmModel: no EDM components given."
  let sms = map (\c -> assembleSubstitutionModel n mps (Just $ snd c)) cs
      edmName = "EDM" ++ show (length cs)
      ws = fromMaybe (map fst cs) mws
      errs = [e | (Left e) <- sms]
  when (length sms /= length ws) $
    error "edmModel: number of substitution models and weights differs."
  if not $ null errs
    then fail $ head errs
    else
      return $
        M.fromSubstitutionModels edmName (V.fromList ws) (V.fromList $ rights sms)

cxxModel :: Maybe [M.Weight] -> Parser M.MixtureModel
cxxModel mws = do
  _ <- char 'C'
  n <- decimal :: Parser Int
  return $ cxx n mws

standardMixtureModel :: [M.Weight] -> Parser M.MixtureModel
standardMixtureModel ws = do
  _ <- string "MIXTURE"
  _ <- char mmStart
  sms <- parseSubstitutionModel `sepBy1` char separator
  _ <- char mmEnd
  -- XXX: The use of `Data.List.NonEmpty.fromList` leads to uninformative error messages.
  return $ M.fromSubstitutionModels "MIXTURE" (V.fromList ws) (V.fromList sms)

mixtureModel ::
  Maybe [EDMComponent] -> Maybe [M.Weight] -> Parser M.MixtureModel
mixtureModel Nothing Nothing =
  try (cxxModel Nothing) <|> fail "No weights provided."
mixtureModel Nothing mws@(Just ws) =
  try (cxxModel mws) <|> standardMixtureModel ws
mixtureModel (Just cs) mws = edmModel cs mws

-- | Parse the phylogenetic model string. The argument list is somewhat long,
-- but models can have many parameters and we have to check for redundant
-- parameters.
--
-- @
-- getPhyloModel maybeSubstitutionModelString maybeMixtureModelString maybeEDMComponents
-- @
getPhyloModel ::
  Maybe String ->
  Maybe String ->
  Maybe [M.Weight] ->
  Maybe [EDMComponent] ->
  Either String P.PhyloModel
getPhyloModel Nothing Nothing _ _ = Left "No model was given. See help."
getPhyloModel (Just _) (Just _) _ _ =
  Left "Both, substitution and mixture model string given; use only one."
getPhyloModel (Just s) Nothing Nothing Nothing =
  Right $
    P.SubstitutionModel $
      parseStringWith
        parseSubstitutionModel
        s
getPhyloModel (Just _) Nothing (Just _) _ =
  Left "Weights given; but cannot be used with substitution model."
getPhyloModel (Just _) Nothing _ (Just _) =
  Left
    "Empirical distribution mixture model components given; but cannot be used with substitution model."
getPhyloModel Nothing (Just m) mws mcs =
  Right $
    P.MixtureModel $
      parseStringWith
        (mixtureModel mcs mws)
        m
