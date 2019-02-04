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
import           Data.Word                                   (Word8)
import           Numeric.LinearAlgebra                       (norm_1, size,
                                                              vector)
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.AminoAcid
import           EvoMod.Data.MarkovProcess.EDMModel
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.Nucleotide
import           EvoMod.Data.MarkovProcess.PhyloModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Tools

type Parser = Parsec Void B.ByteString

bs :: String -> B.ByteString
bs = B.pack

nNuc :: Int
nNuc = cardinality (alphabet DNA)

nAA :: Int
nAA = cardinality (alphabet Protein)

paramsStart :: Word8
paramsStart = c2w '['

paramsEnd :: Word8
paramsEnd = c2w ']'

sdStart :: Word8
sdStart = c2w '{'

sdEnd :: Word8
sdEnd = c2w '}'

name :: Parser String
name = B.unpack <$>
  takeWhile1P (Just "Substitution model name") (`notElem` [paramsStart, paramsEnd])

params :: Parser [Double]
params = between (char paramsStart) (char paramsEnd) (sepBy1 float (char $ c2w ','))

stationaryDistribution :: Parser StationaryDistribution
stationaryDistribution = do
  f <- vector <$> between (char sdStart) (char sdEnd) (sepBy1 float (char $ c2w ','))
  if nearlyEq (norm_1 f) 1.0
    then return f
    else error $ "Sum of stationary distribution is " ++ show (norm_1 f)
         ++ " but should be 1.0."

type Name = String
type Params = [Double]

checkLength :: StationaryDistribution -> Int -> a -> a
checkLength f n r = if size f /= n
                  then error $ "Length of stationary distribution is " ++ show (size f)
                       ++ " but should be " ++ show n ++ "."
                  else r

assembleSubstitutionModel :: Name -> Maybe Params -> Maybe StationaryDistribution
                     -> SubstitutionModel
-- DNA models.
assembleSubstitutionModel "JC" Nothing Nothing = jc
assembleSubstitutionModel "HKY" (Just [k]) (Just f) = checkLength f nNuc $ hky k f
-- Protein models.
assembleSubstitutionModel "LG" Nothing Nothing = lg
assembleSubstitutionModel "LG-Custom" Nothing (Just f) = checkLength f nAA $ lgCustom f
assembleSubstitutionModel "Poisson" Nothing Nothing = poisson
assembleSubstitutionModel "Poisson-Custom" Nothing (Just f)  = checkLength f nAA $ poissonCustom f
assembleSubstitutionModel n mps mf = error . unlines $
  [ "Cannot assemble substitution model. "
  , "Name: " ++ show n
  , "Parameters: " ++ show mps
  , "Stationary distribution: " ++ show mf ]

substitutionModel :: Parser SubstitutionModel
substitutionModel = do
  n  <- name
  mps <- optional params
  mf <- optional stationaryDistribution
  return $ assembleSubstitutionModel n mps mf

parseEDM :: Maybe [EDMComponent] -> Parser MixtureModel
parseEDM mcs = do
  _ <- chunk (bs "EDM")
  _ <- char paramsStart
  n <- name
  mps <- optional params
  _ <- char paramsEnd
  case mcs of
    Nothing -> error "Empirical distributions not given."
    Just cs -> do
      let sms = map (\c -> assembleSubstitutionModel n mps (Just $ cStationaryDistribution c)) cs
          ws = map cWeight cs
          edmName = B.pack $ "EDM" ++ show (length cs)
      return $ MixtureModel edmName
        [ MixtureModelComponent w sm | (w, sm) <- zip ws sms ]

mixtureModel :: Maybe [EDMComponent] -> Parser MixtureModel
mixtureModel = parseEDM

-- | Parse the phylogenetic model string.
phyloModelString :: Maybe [EDMComponent] -> Parser PhyloModel
-- XXX: The EDM components have to be given. Make this more general.
phyloModelString mcs = try (PhyloSubstitutionModel <$> substitutionModel)
                       <|> (PhyloMixtureModel <$> mixtureModel mcs)
