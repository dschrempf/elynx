{- |
Module      :  OptionsTreeAna
Description :  Tree analysis options
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Aug 29 08:16:45 2019.

-}

module OptionsTreeAna
  ( Args (..)
  , parseArgs
  ) where

import           Options.Applicative

import           EvoMod.Tools.Options

data Args = Args
  { argsOutBaseName :: Maybe FilePath
  , argsVerbosity   :: Verbosity
  , argsInFilePath  :: Maybe FilePath }

args :: Parser Args
args = Args <$>
       optional outFileBaseNameOpt <*>
       verbosityOpt <*>
       optional filePathArg

filePathArg :: Parser FilePath
filePathArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read tree(s) from INPUT-FILE"

description :: [String]
description = [ "Analyze phylogenetic trees given in Newick format." ]

parseArgs :: IO Args
parseArgs = parseArgsWith description [] args
