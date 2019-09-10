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

import           Control.Monad.Trans.Reader

import           Options

import           Concatenate.Concatenate
import           Examine.Examine
import           Filter.Filter
import           Simulate.Simulate
import           SubSample.SubSample
import           Translate.Translate

import           ELynx.Tools.Logger
import           ELynx.Tools.Options

main :: IO ()
main = do
  Arguments g c <- parseArguments
  let fn  = outFileBaseName g
      lvl = verbosity g
      lf  = (++ ".log") <$> fn
  case c of
    Concatenate a -> runReaderT   (eLynxWrapper lvl lf concatenateHeader   $ concatenateCmd fn) a
    Examine a -> runReaderT       (eLynxWrapper lvl lf examineHeader       $ examineCmd fn) a
    FilterRows a -> runReaderT    (eLynxWrapper lvl lf filterRowsHeader    $ filterRowsCmd fn) a
    FilterColumns a -> runReaderT (eLynxWrapper lvl lf filterColumnsHeader $ filterColumnsCmd fn) a
    Simulate a -> runReaderT      (eLynxWrapper lvl lf simulateHeader      $ simulateCmd fn) a
    SubSample a -> runReaderT     (eLynxWrapper lvl lf subSampleHeader     $ subSampleCmd fn) a
    Translate a -> runReaderT     (eLynxWrapper lvl lf translateHeader     $ translateCmd fn) a
