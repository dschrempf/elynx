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

import           Compare.Options
import           Connect.Options
import           Distance.Options
import           Examine.Options
import           Simulate.Options

import           ELynx.Tools.Options

data CommandArguments =
  Distance DistanceArguments
  | Examine ExamineArguments
  | Simulate SimulateArguments
  | Compare CompareArguments
  | Connect ConnectArguments

distanceDescription :: String
distanceDescription =  "Compute distances between many phylogenetic trees."

examineDescription :: String
examineDescription  =  "Compute summary statistics of phylogenetic trees."

simulateDescription :: String
simulateDescription =  "Simulate phylogenetic trees using birth and death processes."

compareDescription :: String
compareDescription  =  "Compare two phylogenetic trees (compute distances and branch-wise differences)."

connectDescription :: String
connectDescription  =  "Connect two phylogenetic trees in all ways (possibly honoring constraints)."

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

compareCommand :: Mod CommandFields CommandArguments
compareCommand = command "compare" $
                   info (Compare <$> compareArguments)
                     $ progDesc compareDescription

connectCommand :: Mod CommandFields CommandArguments
connectCommand = command "connect" $
                   info (Connect <$> connectArguments)
                     $ progDesc connectDescription

commandArguments :: Parser CommandArguments
commandArguments = hsubparser $
                   distanceCommand
                   <> examineCommand
                   <> simulateCommand
                   <> compareCommand
                   <> connectCommand

parseArguments :: IO (Arguments CommandArguments)
parseArguments = parseArgumentsWith desc ftr commandArguments

desc :: [String]
desc = [ "Compare, examine, and simulate phylogenetic trees." ]

ftr :: [String]
ftr = "File formats:" : fs
  where
    toListItem = ("  - " ++)
    fs = map toListItem ["Newick"]
