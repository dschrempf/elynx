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

main :: IO ()
main = do
  (Arguments g c) <- parseArguments
  case c of
    Concatenate a ->
      runReaderT (eLynxWrapper concatenateDescription c a concatenateCmd) g
    Examine a -> runReaderT (eLynxWrapper examineDescription c a examineCmd) g
    FilterRows a ->
      runReaderT (eLynxWrapper filterRowsDescription c a filterRowsCmd) g
    FilterCols a ->
      runReaderT (eLynxWrapper filterColumnsDescription c a filterColsCmd) g
    Simulate a -> runReaderT (eLynxWrapper simulateDescription c a simulateCmd) g
    SubSample a ->
      runReaderT (eLynxWrapper subSampleDescription c a subSampleCmd) g
    Translate a ->
      runReaderT (eLynxWrapper translateDescription c a translateCmd) g
