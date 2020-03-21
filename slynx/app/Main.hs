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

import           Options

import           Concatenate.Concatenate
import           Examine.Examine
import           Filter.Filter
import           Simulate.Simulate
import           SubSample.SubSample
import           Translate.Translate

import           ELynx.Tools

main :: IO ()
main = do
  (Arguments g c) <- parseArguments
  case c of
    Concatenate a -> eLynxWrapper concatenateCmd (Arguments g a)
    Examine     a -> eLynxWrapper examineCmd (Arguments g a)
    FilterRows  a -> eLynxWrapper filterRowsCmd (Arguments g a)
    FilterCols  a -> eLynxWrapper filterColsCmd (Arguments g a)
    Simulate    a -> eLynxWrapper simulateCmd (Arguments g a)
    SubSample   a -> eLynxWrapper subSampleCmd (Arguments g a)
    Translate   a -> eLynxWrapper translateCmd (Arguments g a)
