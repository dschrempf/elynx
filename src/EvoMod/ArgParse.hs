{- |
Module      :  ArgParse
Description :  Stuff related to argument parsing.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}


module EvoMod.ArgParse
  ( EvoModIOArgs (..)
  , Command (..)
  , evoModHeader
  , parseEvoModIOArgs
  ) where

import           Control.Applicative
import           Data.Version                    (showVersion)
import           Options.Applicative
import           Options.Applicative.Help.Pretty
import           Paths_EvoMod                    (version)

import           EvoMod.Data.Alphabet

data Command = Summarize
             | Concatenate
             | Filter { longer  :: Maybe Int
                      , shorter :: Maybe Int}

data EvoModIOArgs = EvoModIOArgs
                  {
                    argsCommand     :: Command
                  , argsAlphabet    :: Code
                  , argsFileNameOut :: Maybe String
                  , argsQuiet       :: Bool
                  , argsFileNames   :: [String]
                  }

evolIOOpts :: Parser EvoModIOArgs
evolIOOpts = EvoModIOArgs
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

fileNameOutOpt :: Parser (Maybe String)
fileNameOutOpt = optional $ strOption
  ( long "output-file"
    <> short 'o'
    <> metavar "NAME"
    <> help "Specify output file NAME" )

quietOpt :: Parser Bool
quietOpt = switch
  ( long "quiet"
    <> short 'q'
    <> help "Be quiet")

versionOpt :: Parser (a -> a)
versionOpt = infoOption evoModHeader
  ( long "version"
    <> short 'v'
    <> help "Show version"
    <> hidden )

fileNameArg :: Parser String
fileNameArg = argument str
  ( metavar "INPUT-FILE-NAMES"
    <> help "Read sequences from INPUT-FILE-NAMES" )

evoModVersion :: String
evoModVersion = "EvoMod version " ++ showVersion version ++ "."

evoModCopyright :: String
evoModCopyright = "Developed by Dominik Schrempf."

evoModDescription :: String
evoModDescription = "Parse, view, modify and simulate evolutionary sequences and phylogenetic trees. The goal of EvoMod is reproducible research. Nothing is assumed about the data (e.g., the type of code), and no default values set. Everything has to be stated by the user. This leads to some work overhead in the beginning, but usually pays off in the end."

evoModHeaders :: [String]
evoModHeaders = [ evoModVersion
                , evoModCopyright
                ]

evoModHeader :: String
evoModHeader = unlines evoModHeaders

evoModHeaderDoc :: Doc
evoModHeaderDoc = vcat $ map pretty evoModHeaders

evoModFooters :: [String]
evoModFooters = [ "File formats:" ] ++ fs ++
                [ "", "Alphabet types:" ] ++ as
  where
    toListItem = (" - " ++)
    fs = map toListItem ["FASTA"]
    as = map (toListItem . codeNameVerbose) [(minBound :: Code) ..]

evoModFooterDoc :: Doc
evoModFooterDoc = vcat $ map pretty evoModFooters

-- | Read the arguments and prints out help if needed.
parseEvoModIOArgs :: IO EvoModIOArgs
parseEvoModIOArgs = execParser $
  info (helper <*> versionOpt <*> evolIOOpts)
  (fullDesc
    <> progDesc evoModDescription
    <> headerDoc (Just evoModHeaderDoc)
    <> footerDoc (Just evoModFooterDoc))
