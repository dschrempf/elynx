{- |
Module      :  Filter.Options
Description :  ELynxSeq argument parsing
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}

module Filter.Options
  ( FilterRowsArguments (..)
  , FilterColsArguments (..)
  , filterRowsArguments
  , filterColsArguments
  ) where

import           Control.Applicative
import           Options.Applicative

import           Tools

import           ELynx.Data.Alphabet.Alphabet

-- | Arguments needed for filtering sequences.
data FilterRowsArguments = FilterRowsArguments
  { frAlphabet :: Alphabet
  , frInFile   :: Maybe FilePath
  , frLonger   :: Maybe Int
  , frShorter  :: Maybe Int
  , frStandard :: Bool }

-- | Arguments needed for filtering columns of a multi sequence alignment.
data FilterColsArguments = FilterColsArguments
  { fcAlphabet :: Alphabet
  , fcInFile   :: Maybe FilePath
  , fcStandard :: Maybe Double }

-- | Command line parser.
filterRowsArguments :: Parser FilterRowsArguments
filterRowsArguments = FilterRowsArguments
              <$> alphabetOpt
              <*> optional inFileArg
              <*> filterLongerThanOpt
              <*> filterShorterThanOpt
              <*> filterStandardChars

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

filterStandardChars :: Parser Bool
filterStandardChars = switch $
  long "standard-characters" <>
  help "Only keep sequences containing at least one standard (i.e., non-IUPAC) character"

-- | Command line parser.
filterColsArguments :: Parser FilterColsArguments
filterColsArguments = FilterColsArguments
                 <$> alphabetOpt
                 <*> optional inFileArg
                 <*> filterStandardOpt

filterStandardOpt :: Parser (Maybe Double)
filterStandardOpt = optional $ option auto $
  long "standard-chars" <>
  metavar "DOUBLE" <>
  help "Keep columns with a proportion standard (non-IUPAC) characters larger than DOUBLE in [0,1]"

inFileArg :: Parser FilePath
inFileArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read sequences from INPUT-FILE"
