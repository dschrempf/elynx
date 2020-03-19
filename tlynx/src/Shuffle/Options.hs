{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  Shuffle.Options
Description :  Options for the connect subcommand
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep 19 15:02:21 2019.

-}

module Shuffle.Options
  ( ShuffleArguments(..)
  , shuffleArguments
  )
where

import           Options.Applicative

import           ELynx.Tools.Reproduction

-- | Arguments of shuffle command.
data ShuffleArguments = ShuffleArguments
  { newickIqTreeFlag :: Bool
  , nReplicates      :: Int
  , inFile           :: FilePath
  , argsSeed         :: Seed }
  deriving (Eq, Show, Generic)

instance Reproducible ShuffleArguments where
  inFiles = pure . inFile
  getSeed = Just . argsSeed
  setSeed a s = a { argsSeed = Fixed s }
  parser = shuffleArguments
  description
    = "Shuffle a phylogenetic tree (keep coalescent times, but shuffle topology and leaves)."

instance ToJSON ShuffleArguments

-- | Parse arguments of shuffle command.
shuffleArguments :: Parser ShuffleArguments
shuffleArguments = ShuffleArguments <$> newickIqTree <*> n <*> file <*> seedOpt

newickIqTree :: Parser Bool
newickIqTree = switch $ long "newick-iqtree" <> short 'i' <> help
  "Use IQ-TREE Newick format (internal node labels are branch support values)"

n :: Parser Int
n =
  option auto $ long "replicates" <> short 'n' <> metavar "N" <> value 1 <> help
    "Number of trees to generate"

file :: Parser FilePath
file =
  strArgument $ metavar "TREE-FILE" <> help "File containing a Newick tree"
