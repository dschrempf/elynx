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
  ( Arguments (..)
  , parseArguments
  ) where

import           Options.Applicative

import           ELynx.Tools.Options

data Arguments = Arguments
  { inFile     :: Maybe FilePath
  , globalArgs :: GlobalArguments }

arguments :: Parser Arguments
arguments = Arguments <$>
  optional inFileArg <*>
  globalArguments

inFileArg :: Parser FilePath
inFileArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read trees from INPUT-FILE"

desc :: [String]
desc = [ "Analyze phylogenetic trees." ]

parseArguments :: IO Arguments
parseArguments = parseArgumentsWith desc [] arguments
