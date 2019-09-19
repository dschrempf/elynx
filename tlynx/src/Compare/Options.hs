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


data CompareArguments = CompareArguments
  { argsNormalize  :: Bool
  , argsBranchWise :: Bool
  , argsInFiles    :: [FilePath] }


-- | Logger and reader data type.
type Compare = LoggingT (ReaderT CompareArguments IO)

compareArguments :: Parser CompareArguments
compareArguments = CompareArguments <$>
  normalize
  <*> branchwise
  <*> some file

normalize :: Parser Bool
normalize = switch $
  long "normalize"
  <> short 'n'
  <> help "Normalize trees before comparison"

branchwise :: Parser Bool
branchwise = switch $
  long "branch-wise"
  <> short 'b'
  <> help "Report branch wise differences"

file :: Parser FilePath
file = strArgument $
  metavar "NAME"
  <> help "Tree file"
