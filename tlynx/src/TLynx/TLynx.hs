{- |
Module      :  TLynx.TLynx
Description :  TLynx module
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Apr 23 16:36:43 2020.

-}

module TLynx.TLynx
  ( tlynx
  , rTLynx
  ) where

import           TLynx.Options

import           TLynx.Coalesce.Coalesce
import           TLynx.Compare.Compare
import           TLynx.Connect.Connect
import           TLynx.Distance.Distance
import           TLynx.Examine.Examine
import           TLynx.Shuffle.Shuffle
import           TLynx.Simulate.Simulate

import           ELynx.Tools

-- TODO: Use a class here (e.g., elynx-wrappable) which defines the extractor function.
-- | Run TLynx with given arguments.
tlynx :: Arguments CommandArguments -> IO ()
tlynx c =
  case local c of
    Coalesce _ -> eLynxWrapper c (\(Arguments g (Coalesce l)) -> Arguments g l) coalesce
    Compare  _ -> eLynxWrapper c (\(Arguments g (Compare  l)) -> Arguments g l) compareCmd
    Connect  _ -> eLynxWrapper c (\(Arguments g (Connect  l)) -> Arguments g l) connectCmd
    Distance _ -> eLynxWrapper c (\(Arguments g (Distance l)) -> Arguments g l) distance
    Examine  _ -> eLynxWrapper c (\(Arguments g (Examine  l)) -> Arguments g l) examine
    Shuffle  _ -> eLynxWrapper c (\(Arguments g (Shuffle  l)) -> Arguments g l) shuffleCmd
    Simulate _ -> eLynxWrapper c (\(Arguments g (Simulate l)) -> Arguments g l) simulate

-- | Run TLynx, parse arguments from command line.
rTLynx :: IO ()
rTLynx = parseArguments >>= tlynx
