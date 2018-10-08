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
  , parseEvolIOArgs
  ) where

import           Options.Applicative
import           Options.Applicative.Help.Pretty

import           Evol.Data.Alphabet

data EvolIOArgs = EvolIOArgs
                  { argsFileName :: String
                  , argsAlphabet :: AlphabetName }

evolIOOpts :: Parser EvolIOArgs
evolIOOpts = EvolIOArgs
  <$> fileNameOpt
  <*> alphabetOpt

fileNameOpt :: Parser String
fileNameOpt = argument str
  ( metavar "INPUT-FILE-NAME"
    <> showDefault
    <> help "Read sequences from INPUT-FILE-NAME" )

alphabetOpt :: Parser AlphabetName
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
    <> footerDoc formats )
  where
    formats = Just . vcat $ map pretty strs
    strs   = [ "File formats:"
             , "  - FASTA"
             , ""
             , "Alphabet types:"
             , "  - DNA: Nucleotides"
             , "  - AA:  Amino acids"
             ]
