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
  , concatenateDescription
  , examineDescription
  , filterRowsDescription
  , filterColumnsDescription
  , simulateDescription
  , subSampleDescription
  , translateDescription
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

instance Reproducible CommandArguments where
  inFiles (Concatenate a) = inFiles a
  inFiles (Examine     a) = inFiles a
  inFiles (FilterRows  a) = inFiles a
  inFiles (FilterCols  a) = inFiles a
  inFiles (Simulate    a) = inFiles a
  inFiles (SubSample   a) = inFiles a
  inFiles (Translate   a) = inFiles a

  getSeed (Concatenate a) = getSeed a
  getSeed (Examine     a) = getSeed a
  getSeed (FilterRows  a) = getSeed a
  getSeed (FilterCols  a) = getSeed a
  getSeed (Simulate    a) = getSeed a
  getSeed (SubSample   a) = getSeed a
  getSeed (Translate   a) = getSeed a

  setSeed (Concatenate a) s = Concatenate $ setSeed a s
  setSeed (Examine     a) s = Examine $ setSeed a s
  setSeed (FilterRows  a) s = FilterRows $ setSeed a s
  setSeed (FilterCols  a) s = FilterCols $ setSeed a s
  setSeed (Simulate    a) s = Simulate $ setSeed a s
  setSeed (SubSample   a) s = SubSample $ setSeed a s
  setSeed (Translate   a) s = Translate $ setSeed a s

  parser _ = commandArguments

instance ToJSON CommandArguments

concatenateDescription, examineDescription, filterRowsDescription, filterColumnsDescription, simulateDescription, subSampleDescription, translateDescription
  :: String
concatenateDescription = "Concatenate sequences found in input files."
examineDescription =
  "Examine sequences. If data is a multi sequence alignment, additionally analyze columns."
filterRowsDescription = "Filter rows (or sequences) found in input files."
filterColumnsDescription = "Filter columns of multi-sequence alignments."
simulateDescription = "Simulate multi sequence alignments."
subSampleDescription = "Sub-sample columns from multi sequence alignments."
translateDescription = "Translate from DNA to Protein or DNAX to ProteinX."

concatenateCommand :: Mod CommandFields CommandArguments
concatenateCommand =
  command "concatenate" $ info (Concatenate <$> concatenateArguments) $ progDesc
    concatenateDescription

examineCommand :: Mod CommandFields CommandArguments
examineCommand = command "examine" $ info
  (Examine <$> examineArguments)
  (fullDesc <> progDesc examineDescription)

filterRowsCommand :: Mod CommandFields CommandArguments
filterRowsCommand =
  command "filter-rows" $ info (FilterRows <$> filterRowsArguments) $ progDesc
    filterRowsDescription

filterColumnsCommand :: Mod CommandFields CommandArguments
filterColumnsCommand =
  command "filter-columns"
    $ info (FilterCols <$> filterColsArguments)
    $ progDesc filterColumnsDescription

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = command "simulate" $ info
  (Simulate <$> simulateArguments)
  (fullDesc <> progDesc simulateDescription <> footerDoc
    (Just $ pretty simulateFooter)
  )

subSampleCommand :: Mod CommandFields CommandArguments
subSampleCommand = command "sub-sample" $ info
  (SubSample <$> subSampleArguments)
  (  fullDesc
  <> progDesc subSampleDescription
  <> footer
       "Create a given number of multi sequence alignments, each of which contains a given number of random sites drawn from the original multi sequence alignment."
  )

translateCommand :: Mod CommandFields CommandArguments
translateCommand =
  command "translate" $ info (Translate <$> translateArguments) $ progDesc
    translateDescription

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
