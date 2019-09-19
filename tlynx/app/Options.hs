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
  , distanceDescription
  , examineDescription
  , simulateDescription
  ) where

import           Options.Applicative
import           Options.Applicative.Help.Pretty

import           Distance.Options
import           Examine.Options
import           Simulate.Options

import           ELynx.Tools.Options

data CommandArguments =
  Distance DistanceArguments
  | Examine ExamineArguments
  | Simulate SimulateArguments

distanceDescription, examineDescription, simulateDescription :: String
distanceDescription =  "Compute distances between many phylogenetic trees."
examineDescription  =  "Compute summary statistics of phylogenetic trees."
simulateDescription =  "Simulate phylogenetic trees using birth and death processes."

distanceCommand :: Mod CommandFields CommandArguments
distanceCommand = command "distance" $
                     info (Distance <$> distanceArguments)
                     ( fullDesc
                       <> progDesc distanceDescription
                       <> footerDoc (Just $ pretty distanceFooter) )

examineCommand :: Mod CommandFields CommandArguments
examineCommand = command "examine" $
                     info (Examine <$> examineArguments)
                     $ progDesc examineDescription

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = command "simulate" $
                   info (Simulate <$> simulateArguments)
                     ( fullDesc
                       <> progDesc simulateDescription
                       <> footerDoc (Just $ pretty simulateFooter) )

commandArguments :: Parser CommandArguments
commandArguments = hsubparser $
                   distanceCommand
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
