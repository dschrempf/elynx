{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

{- |
   Description :  Simulate reconstructed trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Tue Feb 27 17:27:16 2018.

See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.

Mon Feb 4 14:26:11 CET 2019: Adding sampling probability rho. See Article
(Stadler2009) Stadler, T. On incomplete sampling under birth–death models and
connections to the sampling-based coalescent Journal of Theoretical Biology,
Elsevier BV, 2009, 261, 58-66

-}

module Simulate.Simulate
  ( simulate
  )
where

import           Control.Concurrent                   (getNumCapabilities)
import           Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8           as L
import           Data.Maybe
import qualified Data.Sequence                        as Seq
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as LT
import qualified Data.Text.Lazy.Encoding              as LT
import           Data.Tree

import           Simulate.Options

import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.PhyloTree            (PhyloLabel)
import           ELynx.Data.Tree.SumStat              (formatNChildSumStat,
                                                       toNChildSumStat)
import           ELynx.Data.Tree.Tree
import           ELynx.Export.Tree.Newick             (toNewick)
import           ELynx.Simulate.PointProcess          (simulateNReconstructedTrees,
                                                       simulateReconstructedTree)
import           ELynx.Tools.Concurrent
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Logger
import           ELynx.Tools.Reproduction             (ELynx, getOutFilePath)

-- | Simulate phylogenetic trees.
simulate :: SimulateArguments -> ELynx ()
simulate a = do
  when (isNothing (argsHeight a) && argsConditionMRCA a) $
    error "Cannot condition on MRCA (-M) when height is not given (-H)."
  let s = argsSumStat a
  c <- liftIO getNumCapabilities
  logNewSection "Arguments"
  $(logInfo) $ T.pack $ reportSimulateArguments a
  logNewSection "Simulation"
  $(logInfo) $ T.pack $ "Number of used cores: " <> show c
  trs <- if argsSubSample a
         then simulateAndSubSampleNTreesConcurrently a
         else simulateNTreesConcurrently a
  let ls = if s
           then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
           else parMap rpar toNewick trs
  fn <- getOutFilePath ".tree"
  let res = L.unlines ls
  out "simulated trees" res fn

simulateNTreesConcurrently :: SimulateArguments -> ELynx [Tree (PhyloLabel Int)]
simulateNTreesConcurrently (SimulateArguments nT nL h cM l m r _ _ s) = do
  let l' = l * r
      m' = m - l * (1.0 - r)
  -- TODO: Reduce duplication.
  c <- liftIO getNumCapabilities
  gs <- liftIO $ getNGen c s
  let chunks = getChunks c nT
      timeSpec = fmap (, cM) h
  trss <- liftIO $ mapConcurrently
          (\(n, g) -> simulateNReconstructedTrees n nL timeSpec l' m' g)
          (zip chunks gs)
  return $ concat trss

simulateAndSubSampleNTreesConcurrently :: SimulateArguments -> ELynx [Tree (PhyloLabel Int)]
simulateAndSubSampleNTreesConcurrently (SimulateArguments nT nL h cM l m r _ _ s) = do
  let nLeavesBigTree = (round $ fromIntegral nL / r) :: Int
  c <- liftIO getNumCapabilities
  gs <- liftIO $ getNGen c s
  let chunks = getChunks c nT
      timeSpec = fmap (, cM) h
  tr <- liftIO $ simulateReconstructedTree nLeavesBigTree timeSpec l m (head gs)
  logNewSection $ T.pack $ "Simulate one big tree with " <> show nLeavesBigTree <> " leaves."
  -- Log the base tree.
  $(logInfo) $ LT.toStrict $ LT.decodeUtf8 $ toNewick tr
  logNewSection $ T.pack $ "Sub sample " <> show nT <> " trees with " <> show nL <> " leaves."
  let lvs = Seq.fromList $ leaves tr
  trss <- liftIO $ mapConcurrently
          (\(nSamples, g) -> nSubSamples nSamples lvs nL tr g)
          (zip chunks gs)
  let trs = catMaybes $ concat trss
  return $ map prune trs
