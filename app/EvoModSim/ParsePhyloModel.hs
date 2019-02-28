{- |
Module      :  ParsePhyloModel
Description :  Parse and interpret the model string
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Feb  1 13:32:16 2019.

TODO: This is super ugly. Isn't there a better way to define models?

Maybe:
-e exchangeabilities
-d stationary distribution
But how to define a mixture model then?

-}

module ParsePhyloModel
  ( phyloModelString
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
import           EvoMod.Data.MarkovProcess.SubstitutionModel    hiding (substitutionModel)
import           EvoMod.Import.MarkovProcess.EDMModelPhylobayes (EDMComponent)
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Equality

type Parser = Parsec Void L.ByteString

bs :: String -> L.ByteString
bs = L.pack

nNuc :: Int
nNuc = cardinalityFromCode DNA

nAA :: Int
nAA = cardinalityFromCode Protein

paramsStart :: Word8
paramsStart = c2w '['

paramsEnd :: Word8
paramsEnd = c2w ']'

sdStart :: Word8
sdStart = c2w '{'

sdEnd :: Word8
sdEnd = c2w '}'

name :: Parser String
name = L.unpack <$>
  takeWhile1P (Just "Substitution model name") (`notElem` [paramsStart, paramsEnd, sdStart])

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

assertLength :: StationaryDistribution -> Int -> a -> a
assertLength f n r = if size f /= n
                    then error $ "Length of stationary distribution is " ++ show (size f)
                         ++ " but should be " ++ show n ++ "."
                    else r

assembleSubstitutionModel :: Name -> Maybe Params -> Maybe StationaryDistribution
                          -> Either String SubstitutionModel
-- DNA models.
assembleSubstitutionModel "JC" Nothing Nothing = Right jc
assembleSubstitutionModel "HKY" (Just [k]) (Just f) = Right $ assertLength f nNuc $ hky k f
-- Protein models.
assembleSubstitutionModel "LG" Nothing Nothing = Right lg
assembleSubstitutionModel "LG-Custom" Nothing (Just f) = Right $ assertLength f nAA $ lgCustom f Nothing
assembleSubstitutionModel "Poisson" Nothing Nothing = Right poisson
assembleSubstitutionModel "Poisson-Custom" Nothing (Just f) = Right $ assertLength f nAA $ poissonCustom f Nothing
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

parseEDM :: [EDMComponent] -> Maybe [Double] -> Parser MixtureModel
parseEDM cs mws = do
  _ <- chunk (bs "EDM")
  _ <- char paramsStart
  n <- name
  mps <- optional params
  _ <- char paramsEnd
  let sms = map (\c -> assembleSubstitutionModel n mps (Just $ snd c)) cs
      edmName = L.pack $ "EDM" ++ show (length cs)
      ws = fromMaybe (map fst cs) mws
  return $ MixtureModel edmName
    [ MixtureModelComponent w sm | (w, Right sm) <- zip ws sms ]

parseCXX :: Maybe [Double] -> Parser MixtureModel
parseCXX mws = do
  n <- name
  case n of
    "C10" -> return $ c10CustomWeights mws
    "C20" -> return $ c20CustomWeights mws
    "C30" -> return $ c30CustomWeights mws
    "C40" -> return $ c40CustomWeights mws
    "C50" -> return $ c50CustomWeights mws
    "C60" -> return $ c60CustomWeights mws
    _     -> fail "Not a CXX model."

mixtureModel :: Maybe [EDMComponent] -> Maybe [Double] -> Parser MixtureModel
mixtureModel Nothing   mws = parseCXX mws
mixtureModel (Just cs) mws = parseEDM cs mws

-- | Parse the phylogenetic model string.
phyloModelString :: Maybe [EDMComponent] -> Maybe [Double] -> Parser PhyloModel
-- XXX: The EDM components have to be given. Make this more general.
phyloModelString mcs mws = try (PhyloSubstitutionModel <$> substitutionModel)
                           <|> (PhyloMixtureModel <$> mixtureModel mcs mws)
