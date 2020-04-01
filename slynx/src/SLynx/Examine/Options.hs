{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  SLynx.Examine.Options
Description :  ELynxSeq argument parsing
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}

module SLynx.Examine.Options
  ( ExamineArguments(..)
  , examineArguments
  )
where

import           Options.Applicative

import           SLynx.Tools

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools

-- | Arguments needed to examine sequences.
data ExamineArguments = ExamineArguments
    { exAlphabet :: Alphabet
    , exInFile   :: FilePath
    , exPerSite  :: Bool }
  deriving (Eq, Show, Generic)

instance Reproducible ExamineArguments where
  inFiles = pure . exInFile
  outSuffixes _ = [".out"]
  getSeed _ = Nothing
-- XXX: Probably throw error when seed is set.
  setSeed = const
  parser  = examineArguments
  cmdName = "examine"
  cmdDesc
    = "Examine sequences. If data is a multi sequence alignment, additionally analyze columns."

instance ToJSON ExamineArguments

-- | Command line parser.
examineArguments :: Parser ExamineArguments
examineArguments =
  ExamineArguments <$> alphabetOpt <*> filePathArg <*> examinePerSiteOpt

examinePerSiteOpt :: Parser Bool
examinePerSiteOpt =
  switch $ long "per-site" <> help "Report per site summary statistics"

filePathArg :: Parser FilePath
filePathArg =
  strArgument $ metavar "INPUT-FILE" <> help "Read sequences from INPUT-FILE"
