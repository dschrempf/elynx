{- |
Module      :  Main
Description :  Work with phylogenetic trees
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep  5 21:53:07 2019.

-}

module Main where

import           Options

import           Coalesce.Coalesce
import           Compare.Compare
import           Connect.Connect
import           Distance.Distance
import           Examine.Examine
import           Shuffle.Shuffle
import           Simulate.Simulate

import           ELynx.Tools

main :: IO ()
main = do
  Arguments g c <- parseArguments
  case c of
    Distance a -> eLynxWrapper distance (Arguments g a)
    Examine  a -> eLynxWrapper examine (Arguments g a)
    Simulate a -> eLynxWrapper simulate (Arguments g a)
    Coalesce a -> eLynxWrapper coalesce (Arguments g a)
    Compare  a -> eLynxWrapper compareCmd (Arguments g a)
    Connect  a -> eLynxWrapper connectCmd (Arguments g a)
    Shuffle  a -> eLynxWrapper shuffleCmd (Arguments g a)
