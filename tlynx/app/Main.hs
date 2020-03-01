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

import           Coalesce.Coalesce
import           Compare.Compare
import           Connect.Connect
import           Distance.Distance
import           Examine.Examine
import           Shuffle.Shuffle
import           Simulate.Simulate

import           ELynx.Tools.Logger

main :: IO ()
main = do
  Arguments g c <- parseArguments
  case c of
    Distance a -> runReaderT (eLynxWrapper distanceDescription a $ distance a) g
    Examine  a -> runReaderT (eLynxWrapper examineDescription a $ examine a) g
    Simulate a -> runReaderT (eLynxWrapper simulateDescription a $ simulate a) g
    Coalesce a -> runReaderT (eLynxWrapper coalesceDescription a $ coalesce a) g
    Compare  a -> runReaderT (eLynxWrapper compareDescription a $ compareCmd a) g
    Connect  a -> runReaderT (eLynxWrapper connectDescription a $ connectCmd a) g
    Shuffle  a -> runReaderT (eLynxWrapper shuffleDescription a $ shuffleCmd a) g
