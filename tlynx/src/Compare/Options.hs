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
  , Compare
  , compareArguments
  ) where

import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Options.Applicative

-- | Arguments of compare command.
data CompareArguments = CompareArguments
  { argsNormalize    :: Bool
  , argsBipartitions :: Bool
  , argsNewickIqTree :: Bool
  , argsInFiles      :: [FilePath] }


-- | Logger and reader data type.
type Compare = LoggingT (ReaderT CompareArguments IO)

-- | Parse arguments of compare command.
compareArguments :: Parser CompareArguments
compareArguments = CompareArguments <$>
  normalize
  <*> bipartitions
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
  <> help "Print common and missing bipartitions"

file :: Parser FilePath
file = strArgument $
  metavar "NAME"
  <> help "Tree file"

newickIqTree :: Parser Bool
newickIqTree = switch $
  long "newick-iqtree"
  <> short 'i'
  <> help "Use IQ-TREE Newick format (internal node labels are branch support values)"
