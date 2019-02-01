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
  ( phyloModelString
  ) where

import qualified Data.ByteString.Lazy.Char8                  as B
import           Data.Void
import           Numeric.LinearAlgebra                       (norm_1, size,
                                                              vector)
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.EDMModel
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.Nucleotide
import           EvoMod.Data.MarkovProcess.PhyloModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Tools

type Parser = Parsec Void B.ByteString

nNuc :: Int
nNuc = cardinality (alphabet DNA)

params :: Parser [Double]
params = do
  _ <- char $ c2w '['
  ps <- sepBy1 float (char $ c2w ',')
  _ <- char $ c2w ']'
  return ps

-- Read a stationary distribution of the form `pi_A,pi_C,pi_G,...`.
stationaryDistribution :: Int -> Parser StationaryDistribution
stationaryDistribution nAlleles = do
  _ <- char $ c2w '['
  f <- vector <$> sepBy float (char $ c2w ',')
  _ <- char $ c2w ']'
  if size f /= nAlleles
    then error "Length of stationary distribution vector is faulty, only DNA models are supported."
  else if nearlyEq (norm_1 f) 1.0 then return f
    else error $ "Stationary distributions sum to " ++ show (norm_1 f) ++ " but should sum to 1.0."

parseJC :: Parser SubstitutionModel
parseJC = chunk (B.pack "JC") >> return jcModel

parseHKY :: Parser SubstitutionModel
parseHKY = do
  _ <- chunk (B.pack "HKY")
  ps <- params
  f  <- stationaryDistribution nNuc
  if length ps /= 1
    then error "HKY model only has one parameter, kappa."
    else return $ hkyModel (head ps) f

-- XXX: More models need to be added.
substitutionModel :: Parser SubstitutionModel
substitutionModel = try parseJC <|> try parseHKY

-- TODO: Code is important! Check if # of aa matches code from
-- substitution model.

-- TODO: Rate matrices need to be initialized with the EDM components.

-- TODO: Think about how models are specified. Probably turn this around.
-- HKY[6.0][EDMFILE]. Otherwise, stationary frequency of HKY model is ignored,
-- which is very bad behavior.
parseEDM :: Maybe [EDMComponent] -> Parser MixtureModel
parseEDM mcs = do
  _ <- chunk (B.pack "EDM")
  _ <- char $ c2w '['
  sm <- substitutionModel
  _ <- char $ c2w ']'
  case mcs of
    Nothing -> error "Empirical distributions not given."
    Just cs -> return $ edmModel sm cs

mixtureModel :: Maybe [EDMComponent] -> Parser MixtureModel
mixtureModel = parseEDM

-- | Parse the phylogenetic model string.
phyloModelString :: Maybe [EDMComponent] -> Parser PhyloModel
-- XXX: The EDM components have to be given. Make this more general.
phyloModelString mcs = try (PhyloSubstitutionModel <$> substitutionModel)
                       <|> (PhyloMixtureModel <$> mixtureModel mcs)
