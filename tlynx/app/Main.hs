{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Main
Description :  Work with phylogenetic trees
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

import           Compare.Compare
import           Examine.Examine
import           Simulate.Simulate

import           ELynx.Tools.Logger
import           ELynx.Tools.Options

main :: IO ()
main = do
  Arguments g c <- parseArguments
  let fn = outFileBaseName g
  let lf = (++ ".log") <$> fn
  case c of
    Compare a  -> runReaderT (eLynxWrapper lf compareHeader  $ compareTrees fn) a
    Examine a  -> runReaderT (eLynxWrapper lf examineHeader  $ examine fn) a
    Simulate a -> runReaderT (eLynxWrapper lf simulateHeader $ simulate fn) a
