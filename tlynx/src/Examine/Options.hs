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
  ( ExamineArguments (..)
  , examineArguments
  ) where

import           Options.Applicative

-- | Arguments needed to examine phylogenetic trees.
data ExamineArguments = ExamineArguments
  { argsInFile       :: Maybe FilePath
  , argsNewickIqTree :: Bool }

-- | Command line parser.
examineArguments :: Parser ExamineArguments
examineArguments = ExamineArguments <$>
  optional inFile
  <*> newickIqTree

inFile :: Parser FilePath
inFile = strArgument $
  metavar "INPUT-FILE" <>
  help "Read trees from INPUT-FILE"

newickIqTree :: Parser Bool
newickIqTree = switch $
  long "newick-iqtree"
  <> short 'i'
  <> help "Use IQ-TREE Newick format (internal node labels are branch support values)"
