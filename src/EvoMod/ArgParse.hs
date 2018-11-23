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
  , parseEvoModIOArgs
  ) where

import           Control.Applicative
import           Options.Applicative
import           Options.Applicative.Help.Pretty

import           EvoMod.Data.Alphabet

data Command = Summarize | Concatenate

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
  concatenateCommand

summarizeCommand :: Mod CommandFields Command
summarizeCommand = command "summarize" $
  info (pure Summarize) (progDesc "Summarize sequences found in input files")

concatenateCommand :: Mod CommandFields Command
concatenateCommand = command "concatenate" $
  info (pure Concatenate) (progDesc "Concatenate sequences found in input files")

alphabetOpt :: Parser Code
alphabetOpt = option auto
  ( long "alphabet"
    <> short 'a'
    <> metavar "NAME"
    <> value DNA
    <> showDefault
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

fileNameArg :: Parser String
fileNameArg = argument str
  ( metavar "INPUT-FILE-NAMES"
    <> help "Read sequences from INPUT-FILE-NAMES" )

-- | Read the arguments and prints out help if needed.
parseEvoModIOArgs :: IO EvoModIOArgs
parseEvoModIOArgs = execParser $
  info (helper <*> evolIOOpts)
  (fullDesc
    <> progDesc "Parse sequence file formats and analyze them."
    <> header "Evolutionary sequences."
    <> footerDoc fo )
  where
    fo = Just . vcat $ map pretty strs
    toListItem = (" - " ++)
    fs = map toListItem ["FASTA"]
    as = map (toListItem . codeNameVerbose) [(minBound :: Code) ..]
    strs   = [ "File formats:" ] ++ fs ++
             [ "", "Alphabet types:" ] ++ as
