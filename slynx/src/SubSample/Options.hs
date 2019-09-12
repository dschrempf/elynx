{- |
Module      :  SubSample.Options
Description :  ELynxSeq argument parsing
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}

module SubSample.Options
  ( SubSampleArguments (..)
  , SubSample
  , subSampleArguments
  ) where

import           Control.Applicative
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.Word
import           Options.Applicative

import           Tools

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools.Options

-- | Data structure holding the Command line arguments.
data SubSampleArguments = SubSampleArguments
    { ssAlphabet    :: Alphabet
    , ssInFile      :: Maybe FilePath
    , ssNSites      :: Int
    , ssNAlignments :: Int
    , ssMbSeed      :: Maybe [Word32] }

-- | The sub sample command can log stuff and read necessary arguments.
type SubSample = LoggingT (ReaderT SubSampleArguments IO)

-- | Sub command parser.
subSampleArguments :: Parser SubSampleArguments
subSampleArguments = SubSampleArguments
                     <$> alphabetOpt
                     <*> optional filePathArg
                     <*> subSampleNSitesOpt
                     <*> subSampleNAlignmentsOpt
                     <*> seedOpt

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

filePathArg :: Parser FilePath
filePathArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read sequences from INPUT-FILE"
