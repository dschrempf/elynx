{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

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
  ( Arguments(..)
  , CommandArguments(..)
  , parseArguments
  )
where

import           Options.Applicative
import           Options.Applicative.Help.Pretty

import           Coalesce.Options
import           Compare.Options
import           Connect.Options
import           Distance.Options
import           Examine.Options
import           Shuffle.Options
import           Simulate.Options

import           ELynx.Tools.Reproduction

data CommandArguments =
  Distance DistanceArguments
  | Examine ExamineArguments
  | Simulate SimulateArguments
  | Coalesce CoalesceArguments
  | Compare CompareArguments
  | Connect ConnectArguments
  | Shuffle ShuffleArguments
  deriving (Eq, Show, Generic)

distanceCommand :: Mod CommandFields CommandArguments
distanceCommand = command "distance" $ info
  (Distance <$> distanceArguments)
  (fullDesc <> progDesc (progHeader @DistanceArguments) <> footerDoc
    (Just $ pretty distanceFooter)
  )

examineCommand :: Mod CommandFields CommandArguments
examineCommand =
  command "examine" $ info (Examine <$> examineArguments) $ progDesc
    (progHeader @ExamineArguments)

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = command "simulate" $ info
  (Simulate <$> simulateArguments)
  (fullDesc <> progDesc (progHeader @SimulateArguments) <> footerDoc
    (Just $ pretty simulateFooter)
  )

coalesceCommand :: Mod CommandFields CommandArguments
coalesceCommand = command "coalesce" $ info
  (Coalesce <$> coalesceArguments)
  (fullDesc <> progDesc (progHeader @CoalesceArguments) <> footerDoc
    (Just $ pretty coalesceFooter)
  )

compareCommand :: Mod CommandFields CommandArguments
compareCommand =
  command "compare" $ info (Compare <$> compareArguments) $ progDesc
    (progHeader @CompareArguments)

connectCommand :: Mod CommandFields CommandArguments
connectCommand =
  command "connect" $ info (Connect <$> connectArguments) $ progDesc
    (progHeader @ConnectArguments)

shuffleCommand :: Mod CommandFields CommandArguments
shuffleCommand =
  command "shuffle" $ info (Shuffle <$> shuffleArguments) $ progDesc
    (progHeader @ShuffleArguments)

commandArguments :: Parser CommandArguments
commandArguments =
  hsubparser
    $  distanceCommand
    <> examineCommand
    <> simulateCommand
    <> coalesceCommand
    <> compareCommand
    <> connectCommand
    <> shuffleCommand

parseArguments :: IO (Arguments CommandArguments)
parseArguments = parseArgumentsWith desc ftr commandArguments

desc :: [String]
desc = ["Compare, examine, and simulate phylogenetic trees."]

ftr :: [String]
ftr = "File formats:" : fs
 where
  toListItem = ("  - " ++)
  fs         = map toListItem ["Newick"]
