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

import Control.Comonad (extend)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebug, logInfo)
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (isNothing)
import Data.Tree (Tree, flatten, rootLabel)
import ELynx.Data.Tree
import ELynx.Export.Tree.Newick (toNewick)
import ELynx.Import.Tree.Newick (oneNewick)
import ELynx.Simulate.PointProcess
  ( PointProcess (PointProcess),
    toReconstructedTree,
  )
import ELynx.Tools
import System.IO (hClose)
import System.Random.MWC (GenIO, initialize)
import TLynx.Shuffle.Options

-- | Shuffle a tree. Get all coalescent times, shuffle them. Get all leaves,
-- shuffle them. Connect the shuffled leaves with the shuffled coalescent times.
-- The shuffled tree has a new topology while keeping the same set of coalescent
-- times and leaves.
shuffleCmd :: ELynx ShuffleArguments ()
shuffleCmd = do
  l <- local <$> ask
  h <- outHandle "results" ".tree"
  let nwF = nwFormat l
  t <- liftIO $ parseFileWith (oneNewick nwF) (inFile l)
  $(logInfo) "Input tree:"
  $(logInfo) $ fromBs $ toNewick t

  -- Check if all branches have a given length. However, the length of the stem is not important.
  let r = rootLabel t
      r' = r {brLen = branchLength $ Just 0}
      t' = t {rootLabel = r'}
  when
    (isNothing $ traverse brLen t')
    ( do
        $(logDebug) $ tShow t'
        error "Not all branches have a given length."
    )

  -- Check if tree is ultrametric enough.
  let dh = sum $ map (height t -) (distancesOriginLeaves t)
  $(logDebug) $ "Distance in branch length to being ultrametric: " <> tShow dh
  when (dh > 2e-4) (error "Tree is not ultrametric.")
  when (dh > eps && dh < 2e-4) $
    $(logInfo)
      "Tree is nearly ultrametric, ignore branch length differences smaller than 2e-4."
  when (dh < eps) $ $(logInfo) "Tree is ultrametric."

  let cs = filter (> 0) $ flatten $ extend rootHeight t
      ls = map getName $ leaves t
  $(logDebug) $ "Number of coalescent times: " <> tShow (length cs)
  $(logDebug) $ "Number of leaves: " <> tShow (length ls)
  $(logDebug) "The coalescent times are: "
  $(logDebug) $ tShow cs

  gen <- case argsSeed l of
    Random -> error "Seed not available; please contact maintainer."
    Fixed s -> liftIO $ initialize s

  ts <- liftIO $ shuffleT (nReplicates l) (height t) cs ls gen
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts

  liftIO $ hClose h

shuffleT ::
  Int -> -- How many?
  Double -> -- Stem length.
  [Double] -> -- Coalescent times.
  [L.ByteString] -> -- Leave names.
  GenIO ->
  IO [Tree (PhyloLabel L.ByteString)]
shuffleT n o cs ls gen = do
  css <- grabble cs n (length cs) gen
  lss <- grabble ls n (length ls) gen
  return
    [ toReconstructedTree "" (PointProcess names times o)
      | (times, names) <- zip css lss
    ]
