{- |
Module      :  Compare.Options
Description :  Options for the compare subcommand
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep 19 15:02:21 2019.

-}

module Compare.Options
  ( CompareArguments (..)
  , compareArguments
  ) where

import           Options.Applicative

-- | Arguments of compare command.
data CompareArguments = CompareArguments
  { argsNormalize    :: Bool
  , argsBipartitions :: Bool
  , argsIntersect    :: Bool
  , argsNewickIqTree :: Bool
  , argsInFiles      :: [FilePath] }

-- | Parse arguments of compare command.
compareArguments :: Parser CompareArguments
compareArguments = CompareArguments <$>
  normalize
  <*> bipartitions
  <*> intersect
  <*> newickIqTree
  <*> some file

normalize :: Parser Bool
normalize = switch $
  long "normalize"
  <> short 'n'
  <> help "Normalize trees before comparison"

bipartitions :: Parser Bool
bipartitions = switch $
  long "bipartitions"
  <> short 'b'
  <> help "Print and plot common and missing bipartitions"

intersect :: Parser Bool
intersect = switch $
  long "intersect"
  <> short 's'
  <> help "Compare intersections; i.e., before comparison, drop leaves that are not present in the other tree"

file :: Parser FilePath
file = strArgument $
  metavar "NAME"
  <> help "Tree file"

newickIqTree :: Parser Bool
newickIqTree = switch $
  long "newick-iqtree"
  <> short 'i'
  <> help "Use IQ-TREE Newick format (internal node labels are branch support values)"
