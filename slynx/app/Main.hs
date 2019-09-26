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
    Concatenate a -> runReaderT (eLynxWrapper lvl lf concatenateDescription   $ concatenateCmd fn) a
    Examine a     -> runReaderT (eLynxWrapper lvl lf examineDescription       $ examineCmd fn) a
    FilterRows a  -> runReaderT (eLynxWrapper lvl lf filterRowsDescription    $ filterRowsCmd fn) a
    FilterCols a  -> runReaderT (eLynxWrapper lvl lf filterColumnsDescription $ filterColsCmd fn) a
    Simulate a    -> runReaderT (eLynxWrapper lvl lf simulateDescription      $ simulateCmd fn) a
    SubSample a   -> runReaderT (eLynxWrapper lvl lf subSampleDescription     $ subSampleCmd fn) a
    Translate a   -> runReaderT (eLynxWrapper lvl lf translateDescription     $ translateCmd fn) a
