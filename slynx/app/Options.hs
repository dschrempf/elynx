{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  Options
Description :  SLynx general options
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sat Sep  7 18:55:03 2019.

-}

module Options
  ( Arguments(..)
  , CommandArguments(..)
  , parseArguments
  )
where

import           Options.Applicative

import           Concatenate.Options
import           Examine.Options
import           Filter.Options
import           Simulate.Options
import           SubSample.Options
import           Translate.Options

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools

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

parseArguments :: IO (Arguments CommandArguments)
parseArguments = parseArgumentsWith desc ftr commandArguments

desc :: [String]
desc = ["Analyze, and simulate multi sequence alignments."]

ftr :: [String]
ftr = ["File formats:"] ++ fs ++ ["", "Alphabet types:"] ++ as
 where
  toListItem = ("  - " ++)
  fs = map toListItem ["FASTA"]
  as = map (toListItem . alphabetDescription) [(minBound :: Alphabet) ..]
