{- |
Module      :  ArgParseSim
Description :  EvoModSim argument parsing.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}


module ArgParseSim
  ( SubstModel (..)
  , EvoModSimArgs (..)
  , parseEvoModSimArgs
  ) where

-- import           Control.Applicative
import           Data.Void
import           Data.Word
import           Numeric.LinearAlgebra             (norm_1, size, vector)
import           Options.Applicative
import           Text.Megaparsec                   hiding (option)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import           EvoMod.ArgParse
import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.RateMatrix.Nucleotide
import           EvoMod.Data.RateMatrix.RateMatrix
import           EvoMod.Tools

type MegaParser = Parsec Void String

-- Ugly convenience function to read in more complicated command line options
-- with megaparsec and optparse
-- (https://github.com/pcapriotti/optparse-applicative#option-readers).
megaReadM :: MegaParser a -> ReadM a
megaReadM p = eitherReader $ \input ->
  let eea = runParser p "" input
  in
    case eea of
      Left eb -> Left $ errorBundlePretty eb
      Right a -> Right a

data SubstModel = SubstModel
  { mCode           :: Code
  , mName           :: String
  , mParams         :: [Double]
  , mStationaryDist :: StationaryDist
  , mRateMatrix     :: RateMatrix
  }

data EvoModSimArgs = EvoModSimArgs
  { argsTreeFile   :: String
  , argsQuiet      :: Bool
  , argsFileOut    :: String
  , argsSubstModel :: SubstModel
  , argsLength     :: Int
  , argsSeed       :: Maybe [Word32]
  }

evoModSimArgs :: Parser EvoModSimArgs
evoModSimArgs = EvoModSimArgs
  <$> treeFileOpt
  <*> quietOpt
  <*> fileOutOpt
  <*> substModelOpt
  <*> lengthOpt
  <*> seedOpt

treeFileOpt :: Parser String
treeFileOpt = strOption
  ( long "tree-file"
    <> short 't'
    <> metavar "NAME"
    <> help "Specify tree file NAME" )

quietOpt :: Parser Bool
quietOpt = switch
  ( long "quiet"
  <> short 'q'
  <> help "Be quiet" )

fileOutOpt :: Parser String
fileOutOpt = strOption
  ( long "output-file"
    <> short 'o'
    <> metavar "NAME"
    <> help "Specify output file NAME")

-- Read a stationary frequency of the form `pi_A,pi_C,pi_G,...`.
parseStateFreq :: Int -> MegaParser StationaryDist
parseStateFreq nAlleles = do
  _ <- char '['
  f <- vector <$> sepBy float (char ',')
  _ <- char ']'
  if size f /= nAlleles
    then error "Length of stationary frequency vector is faulty, only DNA models are supported."
  else if nearlyEq (norm_1 f) 1.0 then return f
    else error $ "Stationary frequencies sum to " ++ show (norm_1 f) ++ " but should sum to 1.0."

parseParams :: MegaParser [Double]
parseParams = do
  _ <- char '['
  params <- sepBy1 float (char ',')
  _ <- char ']'
  return params

parseSubstModel :: MegaParser SubstModel
parseSubstModel = do
  m  <- takeWhile1P (Just "ModelName") (/= '[')
  case m of
       "JC" -> do
         let n = cardinality (alphabet DNA)
             f = vector $ replicate n (1 / fromIntegral n)
         return $ SubstModel DNA m [] f jc
       "HKY" -> do
         let n = cardinality (alphabet DNA)
         ps <- parseParams
         f  <- parseStateFreq n
         if length ps /= 1
           then error "HKY model only has one parameter, kappa."
           else return $ SubstModel DNA m ps f (hky (head ps) f)
       -- "GTR" -> do
       --   ps <- parseParams
       --   f  <- parseStateFreq
       --   if length ps /= 5
       --     then error "GTR model has five parameters."
       --     else return $ GTR (head ps) (ps !! 1) (ps !! 2) (ps !! 3) (ps !! 4) f
       _ -> error "Model string could not be parsed."

substModelOpt :: Parser SubstModel
substModelOpt = option (megaReadM parseSubstModel)
  ( long "substition-model"
    <> short 'm'
    <> metavar "MODEL"
    <> help "Set the substitution model; available models are shown below" )

lengthOpt :: Parser Int
lengthOpt = option auto
  ( long "length"
    <> short 'l'
    <> metavar "NUMBER"
    <> help "Set alignment length to NUMBER" )

-- TODO: Improve reading of seed.
seedOpt :: Parser (Maybe [Word32])
seedOpt = optional $ option auto
  ( long "seed"
    <> short 's'
    <> metavar "INT"
    <> value [ 0 :: Word32 ]
    <> showDefault
    <> help "Set seed for the random number generator" )

-- | Read the arguments and prints out help if needed.
parseEvoModSimArgs :: IO EvoModSimArgs
parseEvoModSimArgs = parseEvoModArgs evoModSimArgs
