{- |
Module      :  OptionsSeqAna
Description :  EvoModSeq argument parsing
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
import           Data.Word
import           Options.Applicative

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Options

data Command = Examine { perSite :: Bool }
             | Concatenate
             | Filter { longer  :: Maybe Int
                      , shorter :: Maybe Int}
             | SubSample { nSites   :: Int
                         , nSamples :: Int
                         , mSeed    :: Maybe [Word32] }

data Args = Args
  {
    argsCode                 :: Code
  , argsMaybeOutFileBaseName :: Maybe FilePath
  , argsVerbosity            :: Verbosity
  , argsCommand              :: Command
  , argsFileNames            :: [FilePath]
  }

args :: Parser Args
args = Args
  <$> alphabetOpt
  <*> optional outFileBaseNameOpt
  <*> verbosityOpt
  <*> commandArg
  <*> some fileNameArg

commandArg :: Parser Command
commandArg = hsubparser $
  examineCommand <>
  -- summarizeCommand <>
  concatenateCommand <>
  filterCommand <>
  subSampleCommand

-- summarizeCommand :: Mod CommandFields Command
-- summarizeCommand = command "summarize" $
--   info (pure Summarize) $ progDesc "Summarize sequences found in input files"

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
  info (Examine <$> examinePerSite) $
  progDesc "Examine sequences; if data is a multi sequence alignment, additionally analyze columns"

-- examineDropNonStandard :: Parser Bool
-- examineDropNonStandard = switch $
--   long "drop-non-standard"
--   <> help "Drop columns in alignment that contain non-standard characters such as gaps or extended IUPAC codes"

examinePerSite :: Parser Bool
examinePerSite = switch $
  long "mean"
  <> help "Report per site summary statistics"

subSampleCommand :: Mod CommandFields Command
subSampleCommand = command "subsample" $
  info (SubSample <$> subSampleNSites <*> subSampleNSamples <*> seedOpt ) $
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
