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
import           Data.List
import           Data.Word
import           Options.Applicative

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Character.Codon
import           EvoMod.Tools.Misc
import           EvoMod.Tools.Options

data Command = Examine { perSite :: Bool }
             | Concatenate
             | Filter { longer  :: Maybe Int
                      , shorter :: Maybe Int}
             | SubSample { nSites   :: Int
                         , nSamples :: Int
                         , mSeed    :: Maybe [Word32] }
             | Translate { readingFrame  :: Int
                         , universalCode :: UniversalCode }

data Args = Args
  {
    argsCode                 :: AlphabetName
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
  subSampleCommand <>
  translateCommand

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
  info (Examine <$> examinePerSiteOpt) $
  progDesc "Examine sequences; if data is a multi sequence alignment, additionally analyze columns"

-- examineDropNonStandard :: Parser Bool
-- examineDropNonStandard = switch $
--   long "drop-non-standard"
--   <> help "Drop columns in alignment that contain non-standard characters such as gaps or extended IUPAC codes"

examinePerSiteOpt :: Parser Bool
examinePerSiteOpt = switch $
  long "mean"
  <> help "Report per site summary statistics"

subSampleCommand :: Mod CommandFields Command
subSampleCommand = command "subsample" $
  info (SubSample <$> subSampleNSitesOpt <*> subSampleNSamplesOpt <*> seedOpt ) $
  progDesc "Sub-sample columns from multi sequence alignments"

subSampleNSitesOpt :: Parser Int
subSampleNSitesOpt = option auto $
  long "number-of-sites"
  <> short 'n'
  <> metavar "INT"
  <> help "Number of sites to randomly sample with replacement"

subSampleNSamplesOpt :: Parser Int
subSampleNSamplesOpt = option auto $
  long "number-of-samples"
  <> short 'm'
  <> metavar "INT"
  <> help "Number of random sub-samples"

translateCommand :: Mod CommandFields Command
translateCommand = command "translate" $
  info (Translate <$> readingFrameOpt <*> universalCodeOpt) $
  progDesc "Translate from DNA to Protein or DNAX to ProteinX"

readingFrameOpt :: Parser Int
readingFrameOpt = option auto $
  long "reading-frame"
  <> short 'r'
  <> metavar "INT"
  <> help "Reading frame [0|1|2]."

universalCodeOpt :: Parser UniversalCode
universalCodeOpt = option auto $
  long "universal-code"
  <> short 'u'
  <> metavar "CODE"
  <> help ("universal code; one of: " ++ codeStr ++ ".")
  where codes = allValues :: [UniversalCode]
        codeWords = map show codes
        codeStr = intercalate ", " codeWords

alphabetOpt :: Parser AlphabetName
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
    as = map (toListItem . alphabetNameVerbose) [(minBound :: AlphabetName) ..]
