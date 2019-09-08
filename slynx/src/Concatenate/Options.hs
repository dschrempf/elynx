{- |
Module      :  Concatenate.Options
Description :  ELynxSeq argument parsing
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}

module Concatenate.Options
  ( ConcatenateArguments (..)
  , Concatenate
  , concatenateCommand
  ) where

import           Control.Applicative
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Options.Applicative

import           Tools

import           ELynx.Data.Alphabet.Alphabet

data ConcatenateArguments = ConcatenateArguments
    { ccAlphabet :: Alphabet
    , ccInFiles  :: [FilePath] }

type Concatenate = LoggingT (ReaderT ConcatenateArguments IO)

concatenateCommand :: Mod CommandFields ConcatenateArguments
concatenateCommand = command "concatenate" $
  info ( ConcatenateArguments <$>
         alphabetOpt <*>
         some inFileArg ) $
  progDesc "Concatenate sequences found in input files."

inFileArg :: Parser FilePath
inFileArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read sequences from INPUT-FILE"
