{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  TLynx.Shuffle.Options
Description :  Options for the connect subcommand
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep 19 15:02:21 2019.

-}

module TLynx.Shuffle.Options
  ( ShuffleArguments(..)
  , shuffleArguments
  )
where

import           Options.Applicative

import           ELynx.Tools

import           TLynx.Parsers

-- | Arguments of shuffle command.
data ShuffleArguments = ShuffleArguments
  { nwFormat    :: NewickFormat
  , nReplicates :: Int
  , inFile      :: FilePath
  , argsSeed    :: Seed }
  deriving (Eq, Show, Generic)

instance Reproducible ShuffleArguments where
  inFiles = pure . inFile
  outSuffixes _ = [".tree"]
  getSeed = Just . argsSeed
  setSeed a s = a { argsSeed = Fixed s }
  parser  = shuffleArguments
  cmdName = "shuffle"
  cmdDesc
    = "Shuffle a phylogenetic tree (keep coalescent times, but shuffle topology and leaves)."

instance ToJSON ShuffleArguments

-- | Parse arguments of shuffle command.
shuffleArguments :: Parser ShuffleArguments
shuffleArguments = ShuffleArguments <$> newickFormat <*> n <*> file <*> seedOpt

n :: Parser Int
n =
  option auto $ long "replicates" <> short 'n' <> metavar "N" <> value 1 <> help
    "Number of trees to generate"

file :: Parser FilePath
file =
  strArgument $ metavar "TREE-FILE" <> help "File containing a Newick tree"
