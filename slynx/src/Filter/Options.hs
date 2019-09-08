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
  , FilterColumnsArguments (..)
  , FilterRows
  , FilterColumns
  , filterRowsArguments
  , filterColumnsArguments
  ) where

import           Control.Applicative
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Options.Applicative

import           Tools

import           ELynx.Data.Alphabet.Alphabet

data FilterRowsArguments = FilterRowsArguments
  { frAlphabet :: Alphabet
  , frInFile     :: Maybe FilePath
  , frLonger   :: Maybe Int
  , frShorter  :: Maybe Int }

data FilterColumnsArguments = FilterColumnsArguments
  { fcAlphabet :: Alphabet
  , fcInFile     :: Maybe FilePath
  , fcStandard :: Maybe Double }

type FilterRows = LoggingT (ReaderT FilterRowsArguments IO)

type FilterColumns = LoggingT (ReaderT FilterColumnsArguments IO)

filterRowsArguments :: Parser FilterRowsArguments
filterRowsArguments = FilterRowsArguments
              <$> alphabetOpt
              <*> optional inFileArg
              <*> filterLongerThanOpt
              <*> filterShorterThanOpt

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

filterColumnsArguments :: Parser FilterColumnsArguments
filterColumnsArguments = FilterColumnsArguments
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
