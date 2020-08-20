-- |
-- Module      :  SLynx.SLynx
-- Description :  SLynx module
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Apr 23 16:38:55 2020.
module SLynx.SLynx
  ( slynx,
    rSLynx,
  )
where

import ELynx.Tools
import SLynx.Concatenate.Concatenate
import SLynx.Examine.Examine
import SLynx.Filter.Filter
import SLynx.Options
import SLynx.Simulate.Simulate
import SLynx.SubSample.SubSample
import SLynx.Translate.Translate

-- | Run SLynx with given arguments.
slynx :: Arguments CommandArguments -> IO ()
slynx c = case local c of
  Concatenate _ ->
    eLynxWrapper
      c
      (\(Arguments g (Concatenate l)) -> Arguments g l)
      concatenateCmd
  Examine _ ->
    eLynxWrapper c (\(Arguments g (Examine l)) -> Arguments g l) examineCmd
  FilterCols _ ->
    eLynxWrapper
      c
      (\(Arguments g (FilterCols l)) -> Arguments g l)
      filterColsCmd
  FilterRows _ ->
    eLynxWrapper
      c
      (\(Arguments g (FilterRows l)) -> Arguments g l)
      filterRowsCmd
  Simulate _ ->
    eLynxWrapper c (\(Arguments g (Simulate l)) -> Arguments g l) simulateCmd
  SubSample _ ->
    eLynxWrapper
      c
      (\(Arguments g (SubSample l)) -> Arguments g l)
      subSampleCmd
  Translate _ ->
    eLynxWrapper
      c
      (\(Arguments g (Translate l)) -> Arguments g l)
      translateCmd

-- | Run SLynx, parse arguments from command line.
rSLynx :: IO ()
rSLynx = parseArguments >>= slynx
