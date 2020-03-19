{-# LANGUAGE DeriveGeneric #-}

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
  ( SubSampleArguments(..)
  , subSampleArguments
  )
where

import           Options.Applicative

import           Tools

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools.Reproduction

-- | Data structure holding the Command line arguments.
data SubSampleArguments = SubSampleArguments
    { ssAlphabet    :: Alphabet
    , ssInFile      :: FilePath
    , ssNSites      :: Int
    , ssNAlignments :: Int
    , ssMbSeed      :: Seed }
  deriving (Eq, Show, Generic)

instance Reproducible SubSampleArguments where
  inFiles = pure . ssInFile
  getSeed = Just . ssMbSeed
  setSeed a s = a { ssMbSeed = Fixed s }
  parser = subSampleArguments
  progHeader = "Sub-sample columns from multi sequence alignments."

instance ToJSON SubSampleArguments

-- | Sub command parser.
subSampleArguments :: Parser SubSampleArguments
subSampleArguments =
  SubSampleArguments
    <$> alphabetOpt
    <*> filePathArg
    <*> subSampleNSitesOpt
    <*> subSampleNAlignmentsOpt
    <*> seedOpt

subSampleNSitesOpt :: Parser Int
subSampleNSitesOpt =
  option auto $ long "number-of-sites" <> short 'n' <> metavar "INT" <> help
    "Number of sites randomly drawn with replacement"

subSampleNAlignmentsOpt :: Parser Int
subSampleNAlignmentsOpt =
  option auto
    $  long "number-of-alignments"
    <> short 'm'
    <> metavar "INT"
    <> help "Number of multi sequence alignments to be created"

filePathArg :: Parser FilePath
filePathArg =
  strArgument $ metavar "INPUT-FILE" <> help "Read sequences from INPUT-FILE"
