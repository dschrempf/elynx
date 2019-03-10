{- |
Module      :  ParsePhyloModel
Description :  Parse and interpret the model string
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Feb  1 13:32:16 2019.

-}

module ParsePhyloModel
  ( getPhyloModel
  ) where

import qualified Data.ByteString.Lazy.Char8                     as L
import           Data.Maybe
import           Data.Void
import           Data.Word                                      (Word8)
import           Numeric.LinearAlgebra                          (norm_1, size,
                                                                 vector)
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.AminoAcid
import           EvoMod.Data.MarkovProcess.CXXModels
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.Nucleotide
import           EvoMod.Data.MarkovProcess.PhyloModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Import.MarkovProcess.EDMModelPhylobayes (EDMComponent)
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Equality
import           EvoMod.Tools.InputOutput

type Parser = Parsec Void L.ByteString

bs :: String -> L.ByteString
bs = L.pack

nNuc :: Int
nNuc = cardinalityFromCode DNA

nAA :: Int
nAA = cardinalityFromCode Protein

-- Model parameters between square brackets.
paramsStart :: Word8
paramsStart = c2w '['

paramsEnd :: Word8
paramsEnd = c2w ']'

-- Stationary distribution between curly brackets.
sdStart :: Word8
sdStart = c2w '{'

sdEnd :: Word8
sdEnd = c2w '}'

-- Mixture model components between round brackets.
mmStart :: Word8
mmStart = c2w '('

mmEnd :: Word8
mmEnd = c2w ')'

name :: Parser String
name = L.unpack <$>
  takeWhile1P (Just "Model name") (`notElem` [paramsStart, paramsEnd, sdStart, sdEnd, mmStart, mmEnd])

params :: Parser [Double]
params = between (char paramsStart) (char paramsEnd) (sepBy1 float (char $ c2w ','))

stationaryDistribution :: Parser StationaryDistribution
stationaryDistribution = do
  f <- vector <$> between (char sdStart) (char sdEnd) (sepBy1 float (char $ c2w ','))
  if nearlyEq (norm_1 f) 1.0
    then return f
    else error $ "Sum of stationary distribution is " ++ show (norm_1 f)
         ++ " but should be 1.0."

assertLength :: StationaryDistribution -> Int -> a -> a
assertLength f n r = if size f /= n
                    then error $ "Length of stationary distribution is " ++ show (size f)
                         ++ " but should be " ++ show n ++ "."
                    else r

-- This is the main function that connects the model string, the parameters and
-- the stationary distribution. It should check that the model is valid.
assembleSubstitutionModel :: String -> Maybe SubstitutionModelParams -> Maybe StationaryDistribution
                          -> Either String SubstitutionModel
-- DNA models.
assembleSubstitutionModel "JC" Nothing Nothing = Right jc
assembleSubstitutionModel "HKY" (Just [k]) (Just f) = Right $ assertLength f nNuc $ hky k f
-- Protein models.
assembleSubstitutionModel "LG" Nothing Nothing = Right lg
assembleSubstitutionModel "LG-Custom" Nothing (Just f) = Right $ assertLength f nAA $ lgCustom f Nothing
assembleSubstitutionModel "Poisson" Nothing Nothing = Right poisson
assembleSubstitutionModel "Poisson-Custom" Nothing (Just f) = Right $ assertLength f nAA $ poissonCustom f Nothing
-- Ohterwisse, we cannot assemble the model.
assembleSubstitutionModel n mps mf = Left $ unlines
  [ "Cannot assemble substitution model. "
  , "Name: " ++ show n
  , "Parameters: " ++ show mps
  , "Stationary distribution: " ++ show mf ]

substitutionModel :: Parser SubstitutionModel
substitutionModel = do
  n  <- name
  mps <- optional params
  mf <- optional stationaryDistribution
  let esm = assembleSubstitutionModel n mps mf
  case esm of
    Left err -> fail err
    Right sm -> return sm

edmModel :: [EDMComponent] -> Maybe [Weight] -> Parser MixtureModel
edmModel cs mws = do
  _ <- chunk (bs "EDM")
  _ <- char mmStart
  n <- name
  mps <- optional params
  _ <- char mmEnd
  let sms = map (\c -> assembleSubstitutionModel n mps (Just $ snd c)) cs
      edmName = L.pack $ "EDM" ++ show (length cs)
      ws = fromMaybe (map fst cs) mws
  return $ MixtureModel edmName
    [ MixtureModelComponent w sm | (w, Right sm) <- zip ws sms ]

cxxModel :: Maybe [Weight] -> Parser MixtureModel
cxxModel mws = do
  n <- name
  case n of
    "C10" -> return $ c10CustomWeights mws
    "C20" -> return $ c20CustomWeights mws
    "C30" -> return $ c30CustomWeights mws
    "C40" -> return $ c40CustomWeights mws
    "C50" -> return $ c50CustomWeights mws
    "C60" -> return $ c60CustomWeights mws
    _     -> fail "Not a CXX model."

-- TODO. MIX(...).
mixtureModel :: Maybe [EDMComponent] -> Maybe [Weight] -> Parser MixtureModel
mixtureModel Nothing   = cxxModel
mixtureModel (Just cs) = edmModel cs

-- | Parse the phylogenetic model string. The argument list is somewhat long,
-- but models can have many parameters and we have to check for redundant
-- parameters.
--
-- @
-- getPhyloModel maybeSubstitutionModelString maybeMixtureModelString maybeEDMComponents
-- @
getPhyloModel :: Maybe String -> Maybe String -> Maybe [Weight] -> Maybe [EDMComponent] -> Either String PhyloModel
getPhyloModel Nothing Nothing _ _              = Left "No model was given. See help."
getPhyloModel (Just _) (Just _) _ _            = Left "Both, substitution and mixture model string given; use only one."
getPhyloModel (Just s) Nothing Nothing Nothing = Right $ PhyloSubstitutionModel $ parseStringWith substitutionModel s
getPhyloModel (Just _) Nothing (Just _) _      = Left "Weights given; but cannot be used with substitution model."
getPhyloModel (Just _) Nothing _ (Just _)      = Left "Empirical distribution mixture model components given; but cannot be used with substitution model."
getPhyloModel Nothing (Just m) mws mcs         = Right $ PhyloMixtureModel $ parseStringWith (mixtureModel mcs mws) m
