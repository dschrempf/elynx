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

import           ELynx.Tools.Options

data CommandArguments =
  Compare CompareArguments
  | Examine ExamineArguments
  | Simulate SimulateArguments

compareCommand :: Mod CommandFields CommandArguments
compareCommand = command "compare" $
                     info (Compare <$> compareArguments)
                     ( fullDesc
                       <> header "Compute distances between phylogenetic trees."
                       <> footerDoc (Just $ pretty compareFooter) )

examineCommand :: Mod CommandFields CommandArguments
examineCommand = command "examine" $
                     info (Examine <$> examineArguments)
                     $ header "Compute summary statistics of phylogenetic trees."

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = command "simulate" $
                   info (Simulate <$> simulateArguments)
                     ( fullDesc
                       <> header "Simulate phylogenetic trees using birth and death processes."
                       <> progDesc simulateDesc
                       <> footerDoc (Just $ pretty simulateFooter) )

commandArguments :: Parser CommandArguments
commandArguments = hsubparser $
                   compareCommand
                   <> examineCommand
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
desc = [ "Compare, examine, and simulate phylogenetic trees." ]

ftr :: [String]
ftr = "File formats:" : fs
  where
    toListItem = ("  - " ++)
    fs = map toListItem ["Newick"]
