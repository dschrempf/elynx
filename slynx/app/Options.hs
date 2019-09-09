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
  , concatenateHeader
  , examineHeader
  , filterRowsHeader
  , filterColumnsHeader
  , simulateHeader
  , subSampleHeader
  , translateHeader
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

concatenateHeader, examineHeader, filterRowsHeader, filterColumnsHeader, simulateHeader, subSampleHeader, translateHeader :: String
concatenateHeader   = "Concatenate sequences found in input files."
examineHeader       = "Examine sequences."
filterRowsHeader    =  "Filter rows (or sequences) found in input files."
filterColumnsHeader = "Filter columns of multi-sequence alignments."
simulateHeader      = "Simulate multi sequence alignments."
subSampleHeader     =  "Sub-sample columns from multi sequence alignments."
translateHeader     =  "Translate from DNA to Protein or DNAX to ProteinX."

concatenateCommand :: Mod CommandFields CommandArguments
concatenateCommand = command "concatenate" $
                     info (Concatenate <$> concatenateArguments)
                     $ header concatenateHeader

examineCommand :: Mod CommandFields CommandArguments
examineCommand = command "examine" $
                     info (Examine <$> examineArguments)
                     ( fullDesc
                       <> header examineHeader
                       <> progDesc "If data is a multi sequence alignment, additionally analyze columns." )

filterRowsCommand :: Mod CommandFields CommandArguments
filterRowsCommand = command "filter-rows" $
                     info (FilterRows <$> filterRowsArguments)
                     $ header filterRowsHeader

filterColumnsCommand :: Mod CommandFields CommandArguments
filterColumnsCommand = command "filter-columns" $
                     info (FilterColumns <$> filterColumnsArguments)
                     $ header filterColumnsHeader

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = command "simulate" $
                  info (Simulate <$> simulateArguments)
                  (fullDesc
                   <> header simulateHeader
                   <> footerDoc (Just $ pretty simulateFooter) )

subSampleCommand :: Mod CommandFields CommandArguments
subSampleCommand = command "sub-sample" $
                   info (SubSample <$> subSampleArguments)
                   ( fullDesc
                     <> header subSampleHeader
                     <> progDesc "Create a given number of multi sequence alignments, each of which containing a given number of random sites drawn from the original multi sequence alignment." )

translateCommand :: Mod CommandFields CommandArguments
translateCommand = command "translate" $
                   info (Translate <$> translateArguments)
                   $ header translateHeader

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
