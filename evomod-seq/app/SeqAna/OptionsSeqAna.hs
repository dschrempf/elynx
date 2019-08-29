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
  , GlobalArgs (..)
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

data Command = Examine
               { exPerSite       :: Bool
               , exMbFp          :: Maybe FilePath }
             | Concatenate
               { ccMbFps         :: [FilePath] }
             | FilterRows
               { frLonger        :: Maybe Int
               , frShorter       :: Maybe Int
               , frMbFp          :: Maybe FilePath }
             | FilterColumns
               { fcStandard      :: Maybe Double
               , fcMbFp          :: Maybe FilePath }
             | SubSample
               { ssNSites        :: Int
               , ssNAlignments   :: Int
               , ssMbSeed        :: Maybe [Word32]
               , ssMbFp          :: Maybe FilePath }
             | Translate
               { trReadingFrame  :: Int
               , trUniversalCode :: UniversalCode
               , trMbFp          :: Maybe FilePath }

data GlobalArgs = GlobalArgs
  { argsAlphabet    :: Alphabet
  , argsOutBaseName :: Maybe FilePath
  , argsVerbosity   :: Verbosity }

data Args = Args
  { argsGlobal  :: GlobalArgs
  , argsCommand :: Command
  }

args :: Parser Args
args = Args <$>
       globalArgs <*>
       commandArg

globalArgs :: Parser GlobalArgs
globalArgs = GlobalArgs <$>
             alphabetOpt <*>
             optional outFileBaseNameOpt <*>
             verbosityOpt

commandArg :: Parser Command
commandArg = hsubparser $
  examineCommand <>
  concatenateCommand <>
  filterRowsCommand <>
  filterColumnsCommand <>
  subSampleCommand <>
  translateCommand

concatenateCommand :: Mod CommandFields Command
concatenateCommand = command "concatenate" $
  info ( Concatenate <$>
         some filePathArg ) $
  progDesc "Concatenate sequences found in input files."

filterRowsCommand :: Mod CommandFields Command
filterRowsCommand = command "filter-rows" $
  info ( FilterRows <$>
         filterLongerThanOpt <*>
         filterShorterThanOpt <*>
         optional filePathArg ) $
  progDesc "Filter rows (or sequences) found in input files."

filterLongerThanOpt :: Parser (Maybe Int)
filterLongerThanOpt = optional $ option auto $
  long "longer-than" <>
  metavar "LENGTH" <>
  help "Only keep sequences longer than LENGTH"

filterShorterThanOpt :: Parser (Maybe Int)
filterShorterThanOpt = optional $ option auto $
  long "shorter-than" <>
  metavar "LENGTH" <>
  help "Only keep sequences shorter than LENGTH"

filterColumnsCommand :: Mod CommandFields Command
filterColumnsCommand = command "filter-columns" $
  info ( FilterColumns <$>
         filterStandardOpt <*>
         optional filePathArg ) $
  progDesc "Filter columns of multi-sequence alignments."

filterStandardOpt :: Parser (Maybe Double)
filterStandardOpt = optional $ option auto $
  long "standard-chars" <>
  metavar "DOUBLE" <>
  help "Keep rows with a proportion standard (non-IUPAC) characters larger than DOUBLE in [0,1]"

examineCommand :: Mod CommandFields Command
examineCommand = command "examine" $
  info ( Examine <$>
        examinePerSiteOpt <*>
        optional filePathArg ) $
  progDesc "Examine sequences. if data is a multi sequence alignment, additionally analyze columns."

examinePerSiteOpt :: Parser Bool
examinePerSiteOpt = switch $
  long "per-site" <>
  help "Report per site summary statistics"

subSampleCommand :: Mod CommandFields Command
subSampleCommand = command "subsample" $
  info ( SubSample <$>
         subSampleNSitesOpt <*>
         subSampleNAlignmentsOpt <*>
         seedOpt <*>
         optional filePathArg ) $
  progDesc "Sub-sample columns from multi sequence alignments. Create a given number of multi sequence alignments, each of which containing a given number of random sites drawn from the original multi sequence alignment."

subSampleNSitesOpt :: Parser Int
subSampleNSitesOpt = option auto $
  long "number-of-sites" <>
  short 'n' <>
  metavar "INT" <>
  help "Number of sites randomly drawn with replacement"

subSampleNAlignmentsOpt :: Parser Int
subSampleNAlignmentsOpt = option auto $
  long "number-of-alignments" <>
  short 'm' <>
  metavar "INT" <>
  help "Number of multi sequence alignments to be created"

translateCommand :: Mod CommandFields Command
translateCommand = command "translate" $
  info ( Translate <$>
         readingFrameOpt <*>
         universalCodeOpt <*>
         optional filePathArg ) $
  progDesc "Translate from DNA to Protein or DNAX to ProteinX."

readingFrameOpt :: Parser Int
readingFrameOpt = option auto $
  long "reading-frame" <>
  short 'r' <>
  metavar "INT" <>
  help "Reading frame [0|1|2]."

universalCodeOpt :: Parser UniversalCode
universalCodeOpt = option auto $
  long "universal-code" <>
  short 'u' <>
  metavar "CODE" <>
  help ("universal code; one of: " ++ codeStr ++ ".")
  where codes = allValues :: [UniversalCode]
        codeWords = map show codes
        codeStr = intercalate ", " codeWords

alphabetOpt :: Parser Alphabet
alphabetOpt = option auto $
  long "alphabet" <>
  short 'a' <>
  metavar "NAME" <>
  help "Specify alphabet type NAME"

filePathArg :: Parser FilePath
filePathArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read sequences from INPUT-FILE"

parseArgs :: IO Args
parseArgs = parseArgsWith desc ftr args

desc :: [String]
desc = [ "Analyze multi sequence alignments." ]

ftr :: [String]
ftr = [ "File formats:" ] ++ fs ++
      [ "", "Alphabet types:" ] ++ as
  where
    toListItem = ("  - " ++)
    fs = map toListItem ["FASTA"]
    as = map (toListItem . alphabetNameVerbose) [(minBound :: Alphabet) ..]
