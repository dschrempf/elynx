{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  Options
Description :  TLynx general options
Copyright   :  (c) Dominik Schrempf 2020
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

import           Coalesce.Options
import           Compare.Options
import           Connect.Options
import           Distance.Options
import           Examine.Options
import           Shuffle.Options
import           Simulate.Options

import           ELynx.Tools

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

parseArguments :: IO (Arguments CommandArguments)
parseArguments = parseArgumentsWith desc ftr commandArguments

desc :: [String]
desc = ["Compare, examine, and simulate phylogenetic trees."]

ftr :: [String]
ftr = "File formats:" : fs
 where
  toListItem = ("  - " ++)
  fs         = map toListItem ["Newick"]
