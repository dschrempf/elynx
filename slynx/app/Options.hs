{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

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
import           Options.Applicative.Help.Pretty

import           Concatenate.Options
import           Examine.Options
import           Filter.Options
import           Simulate.Options
import           SubSample.Options
import           Translate.Options

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools.Reproduction

data CommandArguments =
  Concatenate ConcatenateArguments
  | Examine ExamineArguments
  | FilterRows FilterRowsArguments
  | FilterCols FilterColsArguments
  | Simulate SimulateArguments
  | SubSample SubSampleArguments
  | Translate TranslateArguments
  deriving (Eq, Show, Generic)

-- TODO: This could be simplified.
-- createCommand :: Mod CommandFields a
-- createCommand = command (name @a) $ info (and so on)

concatenateCommand :: Mod CommandFields CommandArguments
concatenateCommand =
  command "concatenate" $ info (Concatenate <$> concatenateArguments) $ progDesc
    (progHeader @ConcatenateArguments)

examineCommand :: Mod CommandFields CommandArguments
examineCommand = command "examine" $ info
  (Examine <$> examineArguments)
  (fullDesc <> progDesc (progHeader @ExamineArguments))

filterRowsCommand :: Mod CommandFields CommandArguments
filterRowsCommand =
  command "filter-rows" $ info (FilterRows <$> filterRowsArguments) $ progDesc
    (progHeader @FilterRowsArguments)

filterColumnsCommand :: Mod CommandFields CommandArguments
filterColumnsCommand =
  command "filter-columns"
    $ info (FilterCols <$> filterColsArguments)
    $ progDesc (progHeader @FilterColsArguments)

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = command "simulate" $ info
  (Simulate <$> simulateArguments)
  (fullDesc <> progDesc (progHeader @SimulateArguments) <> footerDoc
    (Just $ pretty simulateFooter)
  )

subSampleCommand :: Mod CommandFields CommandArguments
subSampleCommand = command "sub-sample" $ info
  (SubSample <$> subSampleArguments)
  (  fullDesc
  <> progDesc (progHeader @SubSampleArguments)
  <> footer
       "Create a given number of multi sequence alignments, each of which contains a given number of random sites drawn from the original multi sequence alignment."
  )

translateCommand :: Mod CommandFields CommandArguments
translateCommand =
  command "translate" $ info (Translate <$> translateArguments) $ progDesc
    (progHeader @TranslateArguments)

commandArguments :: Parser CommandArguments
commandArguments =
  hsubparser
    $  concatenateCommand
    <> examineCommand
    <> filterRowsCommand
    <> filterColumnsCommand
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
  fs         = map toListItem ["FASTA"]
  as         = map (toListItem . description) [(minBound :: Alphabet) ..]
