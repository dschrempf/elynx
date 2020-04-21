{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  TLynx.Examine.Options
Description :  Tree analysis options
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Aug 29 08:16:45 2019.

-}

module TLynx.Examine.Options
  ( ExamineArguments(..)
  , examineArguments
  )
where

import           Options.Applicative

import           ELynx.Tools

-- | Arguments needed to examine phylogenetic trees.
data ExamineArguments = ExamineArguments
  { argsInFile       :: FilePath
  , argsNewickIqTree :: Bool }
  deriving (Eq, Show, Generic)

instance Reproducible ExamineArguments where
  inFiles = pure . argsInFile
  outSuffixes _ = [".out"]
  getSeed _ = Nothing
  setSeed = const
  parser  = examineArguments
  cmdName = "examine"
  cmdDesc = "Compute summary statistics of phylogenetic trees."

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