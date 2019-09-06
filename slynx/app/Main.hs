{- |
Module      :  Main
Description :  Work with molecular sequence data
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep  5 21:53:07 2019.

-}

module Main where

import           Control.Monad.Logger
import           Control.Monad.Trans.Reader

import           Simulate.Options
import           Simulate.Simulate

import           ELynx.Tools.Options

main :: IO ()
main = do
  a <- parseArguments
  let f = outFileBaseName $ globalArgs a
      l = case f of
        Nothing -> runStderrLoggingT simulate
        Just fn -> runFileLoggingT fn simulate
  runReaderT l a
