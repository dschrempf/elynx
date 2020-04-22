{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  SLynx.Options
Description :  SLynx general options
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sat Sep  7 18:55:03 2019.

-}

module SLynx.Options
  ( Arguments(..)
  , CommandArguments(..)
  , parseArguments
  )
where

import           Options.Applicative

import           SLynx.Concatenate.Options
import           SLynx.Examine.Options
import           SLynx.Filter.Options
import           SLynx.Simulate.Options
import           SLynx.SubSample.Options
import           SLynx.Translate.Options

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools

-- | The different SLynx commands and their arguments.
data CommandArguments =
  Concatenate ConcatenateArguments
  | Examine ExamineArguments
  | FilterCols FilterColsArguments
  | FilterRows FilterRowsArguments
  | Simulate SimulateArguments
  | SubSample SubSampleArguments
  | Translate TranslateArguments
  deriving (Eq, Show, Generic)

concatenateCommand :: Mod CommandFields CommandArguments
concatenateCommand = createSubCommand Concatenate

examineCommand :: Mod CommandFields CommandArguments
examineCommand = createSubCommand Examine

filterColumnsCommand :: Mod CommandFields CommandArguments
filterColumnsCommand = createSubCommand FilterCols

filterRowsCommand :: Mod CommandFields CommandArguments
filterRowsCommand = createSubCommand FilterRows

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = createSubCommand Simulate

subSampleCommand :: Mod CommandFields CommandArguments
subSampleCommand = createSubCommand SubSample

translateCommand :: Mod CommandFields CommandArguments
translateCommand = createSubCommand Translate

commandArguments :: Parser CommandArguments
commandArguments =
  hsubparser
    $  concatenateCommand
    <> examineCommand
    <> filterColumnsCommand
    <> filterRowsCommand
    <> simulateCommand
    <> subSampleCommand
    <> translateCommand

-- | Parse SLynx command line.
parseArguments :: IO (Arguments CommandArguments)
parseArguments = parseArgumentsWith desc ftr commandArguments

desc :: [String]
desc = ["Analyze, and simulate multi sequence alignments."]

ftr :: [String]
ftr = ["Available sequence file formats:"] ++ fs ++ ["", "Available alphabets:"] ++ as
 where
  toListItem = ("  - " ++)
  fs = map toListItem ["FASTA"]
  as = map (toListItem . alphabetDescription) (allValues :: [Alphabet])
