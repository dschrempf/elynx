{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  TLynx.Options
Description :  TLynx general options
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sat Sep  7 18:55:03 2019.

-}

module TLynx.Options
  ( Arguments(..)
  , CommandArguments(..)
  , parseArguments
  )
where

import           Options.Applicative

import           TLynx.Coalesce.Options
import           TLynx.Compare.Options
import           TLynx.Connect.Options
import           TLynx.Distance.Options
import           TLynx.Examine.Options
import           TLynx.Shuffle.Options
import           TLynx.Simulate.Options

import           ELynx.Tools

-- | The different TLynx commands and their arguments.
data CommandArguments =
  Coalesce CoalesceArguments
  | Compare CompareArguments
  | Connect ConnectArguments
  | Distance DistanceArguments
  | Examine ExamineArguments
  | Shuffle ShuffleArguments
  | Simulate SimulateArguments
  deriving (Eq, Show, Generic)

coalesceCommand :: Mod CommandFields CommandArguments
coalesceCommand = createSubCommand Coalesce

compareCommand :: Mod CommandFields CommandArguments
compareCommand = createSubCommand Compare

connectCommand :: Mod CommandFields CommandArguments
connectCommand = createSubCommand Connect

distanceCommand :: Mod CommandFields CommandArguments
distanceCommand = createSubCommand Distance

examineCommand :: Mod CommandFields CommandArguments
examineCommand = createSubCommand Examine

shuffleCommand :: Mod CommandFields CommandArguments
shuffleCommand = createSubCommand Shuffle

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = createSubCommand Simulate

commandArguments :: Parser CommandArguments
commandArguments =
  hsubparser
    $  coalesceCommand
    <> compareCommand
    <> connectCommand
    <> distanceCommand
    <> examineCommand
    <> shuffleCommand
    <> simulateCommand

-- | Parse TLynx command line.
parseArguments :: IO (Arguments CommandArguments)
parseArguments = parseArgumentsWith desc ftr commandArguments

desc :: [String]
desc = ["Compare, examine, and simulate phylogenetic trees."]

ftr :: [String]
ftr = "File formats:" : fs
 where
  toListItem = ("  - " ++)
  fs         = map toListItem ["Newick"]
