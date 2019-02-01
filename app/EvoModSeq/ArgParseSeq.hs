{- |
Module      :  ArgParseSeq
Description :  EvoModSeq argument parsing.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}


module ArgParseSeq
  ( EvoModSeqArgs (..)
  , Command (..)
  , parseEvoModSeqArgs
  ) where

import           Control.Applicative
import           Options.Applicative

import           EvoMod.ArgParse
import           EvoMod.Data.Alphabet.Alphabet

data Command = Summarize
             | Concatenate
             | Filter { longer  :: Maybe Int
                      , shorter :: Maybe Int}

data EvoModSeqArgs = EvoModSeqArgs
  { argsCommand     :: Command
  , argsAlphabet    :: Code
  , argsFileNameOut :: Maybe FilePath
  , argsQuiet       :: Bool
  , argsFileNames   :: [FilePath]
  }

evoModSeqArgs :: Parser EvoModSeqArgs
evoModSeqArgs = EvoModSeqArgs
  <$> commandArg
  <*> alphabetOpt
  <*> fileNameOutOpt
  <*> quietOpt
  <*> some fileNameArg

commandArg :: Parser Command
commandArg = hsubparser $
  summarizeCommand <>
  concatenateCommand <>
  filterCommand

summarizeCommand :: Mod CommandFields Command
summarizeCommand = command "summarize" $
  info (pure Summarize) (progDesc "Summarize sequences found in input files")

concatenateCommand :: Mod CommandFields Command
concatenateCommand = command "concatenate" $
  info (pure Concatenate) (progDesc "Concatenate sequences found in input files")

filterCommand :: Mod CommandFields Command
filterCommand = command "filter" $
  info (Filter <$> filterLongerThanOpt
         <*> filterShorterThanOpt)
  (progDesc "Filter sequences found in input files")

filterLongerThanOpt :: Parser (Maybe Int)
filterLongerThanOpt = optional $ option auto
  ( long "longer-than"
    <> metavar "LENGTH"
    <> help "Only keep sequences longer than LENGTH." )

filterShorterThanOpt :: Parser (Maybe Int)
filterShorterThanOpt = optional $ option auto
  ( long "shorter-than"
    <> metavar "LENGTH"
    <> help "Only keep sequences shorter than LENGTH." )

alphabetOpt :: Parser Code
alphabetOpt = option auto
  ( long "alphabet"
    <> short 'a'
    <> metavar "NAME"
    <> help "Specify alphabet type NAME" )

fileNameOutOpt :: Parser (Maybe FilePath)
fileNameOutOpt = optional $ strOption
  ( long "output-file"
    <> short 'o'
    <> metavar "NAME"
    <> help "Specify output file NAME" )

fileNameArg :: Parser FilePath
fileNameArg = argument str
  ( metavar "INPUT-FILE-NAMES"
    <> help "Read sequences from INPUT-FILE-NAMES" )

-- | Option to be quiet.
quietOpt :: Parser Bool
quietOpt = switch
  ( long "quiet"
    <> short 'q'
    <> help "Be quiet" )

parseEvoModSeqArgs :: IO EvoModSeqArgs
parseEvoModSeqArgs = parseEvoModArgs evoModSeqArgs
