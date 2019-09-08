{- |
Module      :  Examine.Options
Description :  ELynxSeq argument parsing
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}

module Examine.Options
  ( ExamineArguments (..)
  , Examine
  , examineArguments
  ) where

import           Control.Applicative
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Options.Applicative

import           Tools

import           ELynx.Data.Alphabet.Alphabet

data ExamineArguments = ExamineArguments
    { exAlphabet :: Alphabet
    , exInFile     :: Maybe FilePath
    , exPerSite  :: Bool }

type Examine = LoggingT (ReaderT ExamineArguments IO)

examineArguments :: Parser ExamineArguments
examineArguments = ExamineArguments
           <$> alphabetOpt
           <*> optional filePathArg
           <*> examinePerSiteOpt

examinePerSiteOpt :: Parser Bool
examinePerSiteOpt = switch $
  long "per-site" <>
  help "Report per site summary statistics"

filePathArg :: Parser FilePath
filePathArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read sequences from INPUT-FILE"
