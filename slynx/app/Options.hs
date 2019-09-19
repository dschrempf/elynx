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
  ( Arguments (..)
  , CommandArguments (..)
  , parseArguments
  , concatenateDescription
  , examineDescription
  , filterRowsDescription
  , filterColumnsDescription
  , simulateDescription
  , subSampleDescription
  , translateDescription
  ) where

import           Options.Applicative
import           Options.Applicative.Help.Pretty

import           Concatenate.Options
import           Examine.Options
import           Filter.Options
import           Simulate.Options
import           SubSample.Options
import           Translate.Options

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools.Options

data CommandArguments =
  Concatenate ConcatenateArguments
  | Examine ExamineArguments
  | FilterRows FilterRowsArguments
  | FilterColumns FilterColumnsArguments
  | Simulate SimulateArguments
  | SubSample SubSampleArguments
  | Translate TranslateArguments

concatenateDescription, examineDescription, filterRowsDescription, filterColumnsDescription, simulateDescription, subSampleDescription, translateDescription :: String
concatenateDescription   = "Concatenate sequences found in input files."
examineDescription       = "Examine sequences. If data is a multi sequence alignment, additionally analyze columns."
filterRowsDescription    =  "Filter rows (or sequences) found in input files."
filterColumnsDescription = "Filter columns of multi-sequence alignments."
simulateDescription      = "Simulate multi sequence alignments."
subSampleDescription     =  "Sub-sample columns from multi sequence alignments."
translateDescription     =  "Translate from DNA to Protein or DNAX to ProteinX."

concatenateCommand :: Mod CommandFields CommandArguments
concatenateCommand = command "concatenate" $
                     info (Concatenate <$> concatenateArguments)
                     $ progDesc concatenateDescription

examineCommand :: Mod CommandFields CommandArguments
examineCommand = command "examine" $
                     info (Examine <$> examineArguments)
                     ( fullDesc
                       <> progDesc examineDescription )

filterRowsCommand :: Mod CommandFields CommandArguments
filterRowsCommand = command "filter-rows" $
                     info (FilterRows <$> filterRowsArguments)
                     $ progDesc filterRowsDescription

filterColumnsCommand :: Mod CommandFields CommandArguments
filterColumnsCommand = command "filter-columns" $
                     info (FilterColumns <$> filterColumnsArguments)
                     $ progDesc filterColumnsDescription

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = command "simulate" $
                  info (Simulate <$> simulateArguments)
                  (fullDesc
                   <> progDesc simulateDescription
                   <> footerDoc (Just $ pretty simulateFooter) )

subSampleCommand :: Mod CommandFields CommandArguments
subSampleCommand = command "sub-sample" $
                   info (SubSample <$> subSampleArguments)
                   ( fullDesc
                     <> progDesc subSampleDescription
                     <> footer "Create a given number of multi sequence alignments, each of which contains a given number of random sites drawn from the original multi sequence alignment." )

translateCommand :: Mod CommandFields CommandArguments
translateCommand = command "translate" $
                   info (Translate <$> translateArguments)
                   $ progDesc translateDescription

commandArguments :: Parser CommandArguments
commandArguments = hsubparser $
                   concatenateCommand
                   <> examineCommand
                   <> filterRowsCommand
                   <> filterColumnsCommand
                   <> simulateCommand
                   <> subSampleCommand
                   <> translateCommand

data Arguments = Arguments
  { globalArgs :: GlobalArguments
  , cmdArgs    :: CommandArguments }

parseArguments :: IO Arguments
parseArguments = parseArgumentsWith desc ftr $
                 Arguments
                 <$> globalArguments
                 <*> commandArguments

desc :: [String]
desc = [ "Analyze, and simulate multi sequence alignments." ]

ftr :: [String]
ftr = [ "File formats:" ] ++ fs ++
      [ "", "Alphabet types:" ] ++ as
  where
    toListItem = ("  - " ++)
    fs = map toListItem ["FASTA"]
    as = map (toListItem . alphabetNameVerbose) [(minBound :: Alphabet) ..]
