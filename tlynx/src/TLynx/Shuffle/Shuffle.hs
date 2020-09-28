{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  TLynx.Shuffle.Shuffle
-- Description :  Shuffle a phylogeny
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Sep 19 15:01:52 2019.
--
-- The coalescent times are unaffected. The topology and the leaf order is
-- shuffled. Branch support values are ignored and lost.
module TLynx.Shuffle.Shuffle
  ( shuffleCmd,
  )
where

import qualified Control.Comonad as C
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebug, logInfo)
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy.Char8 as BL
import ELynx.Tools
import ELynx.Tree
import ELynx.Tree.Simulate.PointProcess
  ( PointProcess (PointProcess),
    toReconstructedTree,
  )
import System.IO (hClose)
import System.Random.MWC (GenIO, initialize)
import TLynx.Shuffle.Options
import TLynx.Parsers

-- | Shuffle a tree. Get all coalescent times, shuffle them. Get all leaves,
-- shuffle them. Connect the shuffled leaves with the shuffled coalescent times.
-- The shuffled tree has a new topology while keeping the same set of coalescent
-- times and leaves.
shuffleCmd :: ELynx ShuffleArguments ()
shuffleCmd = do
  l <- local <$> ask
  h <- outHandle "results" ".tree"
  let nwF = nwFormat l
  tPhylo <- liftIO $ parseTree nwF (inFile l)
  $(logInfo) "Input tree:"
  $(logInfo) $ fromBs $ toNewick tPhylo
  let t = either error id $ phyloToLengthTree tPhylo
  -- Check if tree is ultrametric enough.
  let dh = sum $ map (height t -) (distancesOriginLeaves t)
  $(logDebug) $ "Distance in branch length to being ultrametric: " <> tShow dh
  when (dh > 2e-4) (error "Tree is not ultrametric.")
  when (dh > eps && dh < 2e-4) $
    $(logInfo)
      "Tree is nearly ultrametric, ignore branch length differences smaller than 2e-4."
  when (dh < eps) $ $(logInfo) "Tree is ultrametric."
  let cs = filter (> 0) $ labels $ C.extend rootHeight t
      ls = map getName $ leaves t
  $(logDebug) $ "Number of coalescent times: " <> tShow (length cs)
  $(logDebug) $ "Number of leaves: " <> tShow (length ls)
  $(logDebug) "The coalescent times are: "
  $(logDebug) $ tShow cs
  gen <- case argsSeed l of
    Random -> error "Seed not available; please contact maintainer."
    Fixed s -> liftIO $ initialize s
  ts <- liftIO $ shuffleT (nReplicates l) (height t) cs ls gen
  liftIO $ BL.hPutStr h $ BL.unlines $ map (toNewick . measurableToPhyloTree) ts
  liftIO $ hClose h

shuffleT ::
  Int -> -- How many?
  Double -> -- Stem length.
  [Double] -> -- Coalescent times.
  [BL.ByteString] -> -- Leave names.
  GenIO ->
  IO (Forest Length BL.ByteString)
shuffleT n o cs ls gen = do
  css <- grabble cs n (length cs) gen
  lss <- grabble ls n (length ls) gen
  return
    [ toReconstructedTree "" (PointProcess names times o)
      | (times, names) <- zip css lss
    ]
