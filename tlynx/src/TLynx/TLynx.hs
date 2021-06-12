-- |
-- Module      :  TLynx.TLynx
-- Description :  TLynx module
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Apr 23 16:36:43 2020.
module TLynx.TLynx
  ( tlynx,
    rTLynx,
  )
where

import ELynx.Tools
import TLynx.Compare.Compare
import TLynx.Connect.Connect
import TLynx.Distance.Distance
import TLynx.Examine.Examine
import TLynx.Options
import TLynx.Shuffle.Shuffle
import TLynx.Simulate.Simulate

-- TODO: Use a class here (e.g., elynx-wrappable) which defines the extractor function.

-- | Run TLynx with given arguments.
tlynx :: Arguments CommandArguments -> IO ()
tlynx c = case local c of
  Compare _ ->
    eLynxWrapper c (\(Arguments g (Compare l)) -> Arguments g l) compareCmd
  Connect _ ->
    eLynxWrapper c (\(Arguments g (Connect l)) -> Arguments g l) connectCmd
  Distance _ ->
    eLynxWrapper c (\(Arguments g (Distance l)) -> Arguments g l) distance
  Examine _ ->
    eLynxWrapper c (\(Arguments g (Examine l)) -> Arguments g l) examine
  Shuffle _ ->
    eLynxWrapper c (\(Arguments g (Shuffle l)) -> Arguments g l) shuffleCmd
  Simulate _ ->
    eLynxWrapper c (\(Arguments g (Simulate l)) -> Arguments g l) simulate

-- | Run TLynx, parse arguments from command line.
rTLynx :: IO ()
rTLynx = parseArguments >>= tlynx
