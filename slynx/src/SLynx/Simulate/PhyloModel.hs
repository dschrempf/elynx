{- |
Module      :  SLynx.Simulate.PhyloModel
Description :  Parse and interpret the model string
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Feb  1 13:32:16 2019.

-}

module SLynx.Simulate.PhyloModel
  ( getPhyloModel
  )
where

import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Maybe
import           Data.Void
import           Data.Word                      ( Word8 )
import           Numeric.LinearAlgebra          ( norm_1
                                                , size
                                                , vector
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer

import           ELynx.Data.MarkovProcess.AminoAcid
import           ELynx.Data.MarkovProcess.CXXModels
import qualified ELynx.Data.MarkovProcess.MixtureModel
                                               as M
import           ELynx.Data.MarkovProcess.Nucleotide
import qualified ELynx.Data.MarkovProcess.PhyloModel
                                               as P
import           ELynx.Data.MarkovProcess.RateMatrix
import qualified ELynx.Data.MarkovProcess.SubstitutionModel
                                               as S
import           ELynx.Import.MarkovProcess.EDMModelPhylobayes
                                                ( EDMComponent )
import           ELynx.Tools

type Parser = Parsec Void L.ByteString

bs :: String -> L.ByteString
bs = L.pack

nNuc :: Int
-- nNuc = length (alphabet :: [Nucleotide])
nNuc = 4

nAA :: Int
-- nAA = length (alphabet :: [AminoAcid])
nAA = 20

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

separator :: Word8
separator = c2w ','

name :: Parser String
name = L.unpack <$> takeWhile1P
  (Just "Model name")
  (`notElem` [paramsStart, paramsEnd, sdStart, sdEnd, mmStart, mmEnd, separator]
  )

params :: Parser [Double]
params =
  between (char paramsStart) (char paramsEnd) (sepBy1 float (char separator))

stationaryDistribution :: Parser StationaryDistribution
stationaryDistribution = do
  f <- vector
    <$> between (char sdStart) (char sdEnd) (sepBy1 float (char separator))
  if nearlyEq (norm_1 f) 1.0
    then return f
    else
      error
      $  "Sum of stationary distribution is "
      ++ show (norm_1 f)
      ++ " but should be 1.0."

assertLength :: StationaryDistribution -> Int -> a -> a
assertLength d n r = if size d /= n
  then
    error
    $  "Length of stationary distribution is "
    ++ show (size d)
    ++ " but should be "
    ++ show n
    ++ "."
  else r

-- This is the main function that connects the model string, the parameters and
-- the stationary distribution. It should check that the model is valid.
assembleSubstitutionModel
  :: String
  -> Maybe S.Params
  -> Maybe StationaryDistribution
  -> Either String S.SubstitutionModel
-- DNA models.
assembleSubstitutionModel "JC" Nothing Nothing = Right jc
assembleSubstitutionModel "F81" Nothing (Just d) =
  Right $ assertLength d nNuc $ f81 d
assembleSubstitutionModel "HKY" (Just [k]) (Just d) =
  Right $ assertLength d nNuc $ hky k d
-- Protein models.
assembleSubstitutionModel "LG" Nothing Nothing = Right lg
assembleSubstitutionModel "LG-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ lgCustom Nothing d
assembleSubstitutionModel "WAG" Nothing Nothing = Right wag
assembleSubstitutionModel "WAG-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ wagCustom Nothing d
assembleSubstitutionModel "Poisson" Nothing Nothing = Right poisson
assembleSubstitutionModel "Poisson-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ poissonCustom Nothing d
-- Ohterwisse, we cannot assemble the model.
assembleSubstitutionModel n mps mf = Left $ unlines
  [ "Cannot assemble substitution model."
  , "Name: " ++ show n
  , "Parameters: " ++ show mps
  , "Stationary distribution: " ++ show mf
  ]

parseSubstitutionModel :: Parser S.SubstitutionModel
parseSubstitutionModel = do
  n   <- name
  mps <- optional params
  mf  <- optional stationaryDistribution
  let esm = assembleSubstitutionModel n mps mf
  case esm of
    Left  err -> fail err
    Right sm  -> return sm

edmModel :: [EDMComponent] -> Maybe [M.Weight] -> Parser M.MixtureModel
edmModel cs mws = do
  _   <- chunk (bs "EDM")
  _   <- char mmStart
  n   <- name
  mps <- optional params
  _   <- char mmEnd
  let sms     = map (\c -> assembleSubstitutionModel n mps (Just $ snd c)) cs
      edmName = "EDM" ++ show (length cs)
      ws      = fromMaybe (map fst cs) mws
      errs    = [ e | (Left e) <- sms ]
  if not $ null errs
    then fail $ head errs
    else return $ M.MixtureModel
      edmName
      [ M.Component w sm | (w, Right sm) <- zip ws sms ]

cxxModel :: Maybe [M.Weight] -> Parser M.MixtureModel
cxxModel mws = do
  _ <- char (c2w 'C')
  n <- decimal :: Parser Int
  case cxx n mws of
    Nothing -> fail "Only 10, 20, 30, 40, 50, and 60 components are supported."
    Just m  -> return m

standardMixtureModel :: [M.Weight] -> Parser M.MixtureModel
standardMixtureModel ws = do
  _   <- chunk (bs "MIXTURE")
  _   <- char mmStart
  sms <- parseSubstitutionModel `sepBy1` char separator
  _   <- char mmEnd
  return $ M.MixtureModel "MIXTURE" [ M.Component w sm | (w, sm) <- zip ws sms ]

mixtureModel
  :: Maybe [EDMComponent] -> Maybe [M.Weight] -> Parser M.MixtureModel
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
getPhyloModel
  :: Maybe String
  -> Maybe String
  -> Maybe [M.Weight]
  -> Maybe [EDMComponent]
  -> Either String P.PhyloModel
getPhyloModel Nothing Nothing _ _ = Left "No model was given. See help."
getPhyloModel (Just _) (Just _) _ _ =
  Left "Both, substitution and mixture model string given; use only one."
getPhyloModel (Just s) Nothing Nothing Nothing =
  Right $ P.SubstitutionModel $ parseStringWith "Substitution model string"
                                                parseSubstitutionModel
                                                s
getPhyloModel (Just _) Nothing (Just _) _ =
  Left "Weights given; but cannot be used with substitution model."
getPhyloModel (Just _) Nothing _ (Just _) =
  Left
    "Empirical distribution mixture model components given; but cannot be used with substitution model."
getPhyloModel Nothing (Just m) mws mcs =
  Right $ P.MixtureModel $ parseStringWith "Mixture model string"
                                           (mixtureModel mcs mws)
                                           m
