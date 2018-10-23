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


module Evol.ArgParse
  ( EvolIOArgs (..)
  , Command (..)
  , parseEvolIOArgs
  ) where

import           Options.Applicative
import           Options.Applicative.Help.Pretty

import           Evol.Data.Alphabet

data Command = Summarize | Concatenate

data EvolIOArgs = EvolIOArgs
                  {
                    argsCommand   :: Command
                  , argsAlphabet  :: Code
                  , argsFileNames :: [String]
                  }

evolIOOpts :: Parser EvolIOArgs
evolIOOpts = EvolIOArgs
  <$> commandArg
  <*> alphabetOpt
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

fileNameArg :: Parser String
fileNameArg = argument str
  ( metavar "INPUT-FILE-NAMES"
    <> help "Read sequences from INPUT-FILE-NAMES" )

alphabetOpt :: Parser Code
alphabetOpt = option auto
  ( long "alphabet"
    <> short 'a'
    <> metavar "NAME"
    <> value DNA
    <> showDefault
    <> help "Specify alphabet type NAME" )

-- | Read the arguments and prints out help if needed.
parseEvolIOArgs :: IO EvolIOArgs
parseEvolIOArgs = execParser $
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
