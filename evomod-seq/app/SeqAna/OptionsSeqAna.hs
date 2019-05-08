{- |
Module      :  OptionsSeqAna
Description :  EvoModSeq argument parsing.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}

module OptionsSeqAna
  ( Args (..)
  , Command (..)
  , parseArgs
  ) where

import           Control.Applicative
import           Options.Applicative

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Options

data Command = Summarize
             | Concatenate
             | Filter { longer  :: Maybe Int
                      , shorter :: Maybe Int}
             | Examine { drop :: Bool
                       , mean :: Bool }
             | SubSample { nSites   :: Int
                         , nSamples :: Int }

data Args = Args
  {
    argsCode             :: Code
  , argsMaybeFileNameOut :: Maybe FilePath
  , argsVerbosity        :: Verbosity
  , argsCommand          :: Command
  , argsFileNames        :: [FilePath]
  }

args :: Parser Args
args = Args
  <$> alphabetOpt
  <*> optional fileNameOutOpt
  <*> verbosityOpt
  <*> commandArg
  <*> some fileNameArg

commandArg :: Parser Command
commandArg = hsubparser $
  summarizeCommand <>
  concatenateCommand <>
  filterCommand <>
  examineCommand <>
  subSampleCommand

summarizeCommand :: Mod CommandFields Command
summarizeCommand = command "summarize" $
  info (pure Summarize) $ progDesc "Summarize sequences found in input files"

concatenateCommand :: Mod CommandFields Command
concatenateCommand = command "concatenate" $
  info (pure Concatenate) $ progDesc "Concatenate sequences found in input files"

filterCommand :: Mod CommandFields Command
filterCommand = command "filter" $
  info (Filter <$> filterLongerThanOpt
         <*> filterShorterThanOpt) $
  progDesc "Filter sequences found in input Files"

filterLongerThanOpt :: Parser (Maybe Int)
filterLongerThanOpt = optional $ option auto $
  long "longer-than"
  <> metavar "LENGTH"
  <> help "Only keep sequences longer than LENGTH"

filterShorterThanOpt :: Parser (Maybe Int)
filterShorterThanOpt = optional $ option auto $
  long "shorter-than"
  <> metavar "LENGTH"
  <> help "Only keep sequences shorter than LENGTH"

examineCommand :: Mod CommandFields Command
examineCommand = command "examine" $
  info (Examine <$> examineDropNonStandard <*> examineMean) $
  progDesc "Examine columns of multi sequence alignments"

examineDropNonStandard :: Parser Bool
examineDropNonStandard = switch $
  long "drop-non-standard"
  <> help "Drop columns in alignment that contain non-standard characters such as gaps or IUPAC codes"

examineMean :: Parser Bool
examineMean = switch $
  long "mean"
  <> help "Only report mean values"

subSampleCommand :: Mod CommandFields Command
subSampleCommand = command "subsample" $
  info (SubSample <$> subSampleNSites <*> subSampleNSamples) $
  progDesc "Sub-sample columns from multi sequence alignments"

subSampleNSites :: Parser Int
subSampleNSites = option auto $
  long "number-of-sites"
  <> short 'n'
  <> metavar "INT"
  <> help "Number of sites to randomly sample with replacement"

subSampleNSamples :: Parser Int
subSampleNSamples = option auto $
  long "number-of-samples"
  <> short 'm'
  <> metavar "INT"
  <> help "Number of random sub-samples"

alphabetOpt :: Parser Code
alphabetOpt = option auto $
  long "alphabet"
  <> short 'a'
  <> metavar "NAME"
  <> help "Specify alphabet type NAME"

fileNameArg :: Parser FilePath
fileNameArg = argument str $
  metavar "INPUT-FILE-NAMES"
  <> help "Read sequences from INPUT-FILE-NAMES"

parseArgs :: IO Args
parseArgs = parseArgsWith Nothing (Just ftr) args

ftr :: [String]
ftr = [ "File formats:" ] ++ fs ++
      [ "", "Alphabet types:" ] ++ as
  where
    toListItem = ("  - " ++)
    fs = map toListItem ["FASTA"]
    as = map (toListItem . codeNameVerbose) [(minBound :: Code) ..]
