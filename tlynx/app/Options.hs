{- |
Module      :  Options
Description :  TLynx general options
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
  ) where

import           Options.Applicative
import           Options.Applicative.Help.Pretty

import           Compare.Options
import           Examine.Options
import           Simulate.Options

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools.Options

data CommandArguments =
  Compate CompareArguments
  | Examine ExamineArguments
  | Simulate SimulateArguments

examineCommand :: Mod CommandFields CommandArguments
examineCommand = command "examine" $
                     info (Examine <$> examineArguments)
                     $ progDesc "Examine sequences. if data is a multi sequence alignment, additionally analyze columns."

subSampleCommand :: Mod CommandFields CommandArguments
subSampleCommand = command "sub-sample" $
                   info (SubSample <$> subSampleArguments)
                   $ progDesc "Sub-sample columns from multi sequence alignments. Create a given number of multi sequence alignments, each of which containing a given number of random sites drawn from the original multi sequence alignment."

commandArguments :: Parser CommandArguments
commandArguments = hsubparser $
                   examineCommand
                   <> compareCommand
                   <> simulateCommand

data Arguments = Arguments
  { globalArgs :: GlobalArguments
  , cmdArgs    :: CommandArguments }

parseArguments :: IO Arguments
parseArguments = parseArgumentsWith desc ftr $
                 Arguments
                 <$> globalArguments
                 <*> commandArguments

desc :: [String]
desc = [ "Analyze phylogenetic trees." ]

ftr :: [String]
ftr = [ "File formats:" ] ++ fs
  where
    toListItem = ("  - " ++)
    fs = map toListItem ["Newick"]
