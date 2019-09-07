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
  , subSampleCommand
  , Seq
  ) where

import           Control.Applicative
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.Word
import           Options.Applicative

import           ELynx.Tools.Options

data SubSampleArguments = SubSampleArguments
    { ssNSites      :: Int
    , ssNAlignments :: Int
    , ssMbSeed      :: Maybe [Word32]
    , ssMbFp        :: Maybe FilePath }

type Seq = LoggingT (ReaderT SubSampleArguments IO)

subSampleCommand :: Mod CommandFields SubSampleArguments
subSampleCommand = command "subsample" $
  info ( SubSampleArguments <$>
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

filePathArg :: Parser FilePath
filePathArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read sequences from INPUT-FILE"
