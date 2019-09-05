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

import           ELynx.Tools.Options

data Args = Args
  { argsOutFileBaseName :: Maybe FilePath
  , argsVerbosity       :: Verbosity
  , argsInFilePath      :: Maybe FilePath }

args :: Parser Args
args = Args <$>
       optional outFileBaseNameOpt <*>
       verbosityOpt <*>
       optional filePathArg

filePathArg :: Parser FilePath
filePathArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read tree(s) from INPUT-FILE"

desc :: [String]
desc = [ "Analyze phylogenetic trees." ]

parseArgs :: IO Args
parseArgs = parseArgsWith desc [] args
