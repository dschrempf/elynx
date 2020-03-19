{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  Examine.Options
Description :  Tree analysis options
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Aug 29 08:16:45 2019.

-}

module Examine.Options
  ( ExamineArguments(..)
  , examineArguments
  )
where

import           Options.Applicative

import           ELynx.Tools.Reproduction

-- | Arguments needed to examine phylogenetic trees.
data ExamineArguments = ExamineArguments
  { argsInFile       :: FilePath
  , argsNewickIqTree :: Bool }
  deriving (Eq, Show, Generic)

instance Reproducible ExamineArguments where
  inFiles = pure . argsInFile
  getSeed _ = Nothing
  setSeed     = const
  parser      = examineArguments
  description = "Compute summary statistics of phylogenetic trees."

instance ToJSON ExamineArguments

-- | Command line parser.
examineArguments :: Parser ExamineArguments
examineArguments = ExamineArguments <$> inFile <*> newickIqTree

inFile :: Parser FilePath
inFile =
  strArgument $ metavar "INPUT-FILE" <> help "Read trees from INPUT-FILE"

newickIqTree :: Parser Bool
newickIqTree = switch $ long "newick-iqtree" <> short 'i' <> help
  "Use IQ-TREE Newick format (internal node labels are branch support values)"
