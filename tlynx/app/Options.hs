{-# LANGUAGE DeriveGeneric #-}

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
  , distanceDescription
  , examineDescription
  , simulateDescription
  , coalesceDescription
  , compareDescription
  , connectDescription
  , shuffleDescription
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

-- TODO: Is there no easier way?
instance Reproducible CommandArguments where
  inFiles (Distance a) = inFiles a
  inFiles (Examine  a) = inFiles a
  inFiles (Simulate a) = inFiles a
  inFiles (Coalesce a) = inFiles a
  inFiles (Compare  a) = inFiles a
  inFiles (Connect  a) = inFiles a
  inFiles (Shuffle  a) = inFiles a

  getSeed (Distance a) = getSeed a
  getSeed (Examine  a) = getSeed a
  getSeed (Simulate a) = getSeed a
  getSeed (Coalesce a) = getSeed a
  getSeed (Compare  a) = getSeed a
  getSeed (Connect  a) = getSeed a
  getSeed (Shuffle  a) = getSeed a

  setSeed (Distance a) s = Distance (setSeed a s)
  setSeed (Examine  a) s = Examine (setSeed a s)
  setSeed (Simulate a) s = Simulate (setSeed a s)
  setSeed (Coalesce a) s = Coalesce (setSeed a s)
  setSeed (Compare  a) s = Compare (setSeed a s)
  setSeed (Connect  a) s = Connect (setSeed a s)
  setSeed (Shuffle  a) s = Shuffle (setSeed a s)

  parser _ = commandArguments

instance ToJSON CommandArguments

distanceDescription :: String
distanceDescription = "Compute distances between many phylogenetic trees."

examineDescription :: String
examineDescription = "Compute summary statistics of phylogenetic trees."

simulateDescription :: String
simulateDescription =
  "Simulate phylogenetic trees using birth and death processes (see also the 'coalesce' command for simulations using the coalescent process)."

coalesceDescription :: String
coalesceDescription =
  "Simulate phylogenetic trees using the coalescent processes (see also the 'simulate' command for simulations using the birth and death process)."

compareDescription :: String
compareDescription =
  "Compare two phylogenetic trees (compute distances and branch-wise differences)."

connectDescription :: String
connectDescription =
  "Connect two phylogenetic trees in all ways (possibly honoring constraints)."

shuffleDescription :: String
shuffleDescription =
  "Shuffle a phylogenetic tree (keep coalescent times, but shuffle topology and leaves)."

distanceCommand :: Mod CommandFields CommandArguments
distanceCommand = command "distance" $ info
  (Distance <$> distanceArguments)
  (fullDesc <> progDesc distanceDescription <> footerDoc
    (Just $ pretty distanceFooter)
  )

examineCommand :: Mod CommandFields CommandArguments
examineCommand =
  command "examine" $ info (Examine <$> examineArguments) $ progDesc
    examineDescription

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = command "simulate" $ info
  (Simulate <$> simulateArguments)
  (fullDesc <> progDesc simulateDescription <> footerDoc
    (Just $ pretty simulateFooter)
  )

coalesceCommand :: Mod CommandFields CommandArguments
coalesceCommand = command "coalesce" $ info
  (Coalesce <$> coalesceArguments)
  (fullDesc <> progDesc coalesceDescription <> footerDoc
    (Just $ pretty coalesceFooter)
  )

compareCommand :: Mod CommandFields CommandArguments
compareCommand =
  command "compare" $ info (Compare <$> compareArguments) $ progDesc
    compareDescription

connectCommand :: Mod CommandFields CommandArguments
connectCommand =
  command "connect" $ info (Connect <$> connectArguments) $ progDesc
    connectDescription

shuffleCommand :: Mod CommandFields CommandArguments
shuffleCommand =
  command "shuffle" $ info (Shuffle <$> shuffleArguments) $ progDesc
    shuffleDescription

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
