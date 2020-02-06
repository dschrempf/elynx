{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

{- |
   Description :  Simulate reconstructed trees using the coalescent process
   Copyright   :  (c) Dominik Schrempf 2020
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Tue Feb 3 17:00:00 2020.

-}

module Coalesce.Coalesce
  ( coalesce
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

import           Coalesce.Options

import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.PhyloTree            (PhyloLabel)
import           ELynx.Data.Tree.SumStat              (formatNChildSumStat,
                                                       toNChildSumStat)
import           ELynx.Data.Tree.Tree
import           ELynx.Export.Tree.Newick             (toNewick)
import           ELynx.Simulate.Coalescent            (simulate)

import           ELynx.Tools.Concurrent
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Logger
import           ELynx.Tools.Reproduction

-- | Simulate phylogenetic trees.
coalesce :: CoalesceArguments -> ELynx ()
coalesce a = do
  let s = argsSumStat a
  logNewSection "Arguments"
  $(logInfo) $ T.pack $ reportCoalesceArguments a
  logNewSection "Simulation"
  c <- liftIO getNumCapabilities
  $(logInfo) $ T.pack $ "Number of used cores: " <> show c
  trs <- case argsRho a of
           Nothing -> simulateNTreesConcurrently a
           Just _  -> simulateAndSubSampleNTreesConcurrently a
  let ls = if s
           then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
           else parMap rpar toNewick trs
  fn <- getOutFilePath ".tree"
  let res = L.unlines ls
  out "simulated trees" res fn

simulateNTreesConcurrently :: CoalesceArguments -> ELynx [Tree (PhyloLabel Int)]
simulateNTreesConcurrently (CoalesceArguments nT nL _ _ s) = do
  c <- liftIO getNumCapabilities
  gs <- liftIO $ getNGen c s
  let chunks = getChunks c nT
  trss <- liftIO $ mapConcurrently
          (\(n, g) -> replicateM n $ simulate nL g)
          (zip chunks gs)
  return $ concat trss

simulateAndSubSampleNTreesConcurrently :: CoalesceArguments -> ELynx [Tree (PhyloLabel Int)]
simulateAndSubSampleNTreesConcurrently (CoalesceArguments nT nL mR _ s) = do
  c <- liftIO getNumCapabilities
  let r = fromMaybe
          (error "cimulateAndSubSampleNTreesConcurrently: no sampling probability given.")
          mR
  let nLeavesBigTree = (round $ fromIntegral nL / r) :: Int
  gs <- liftIO $ getNGen c s
  let chunks = getChunks c nT
  tr <- liftIO $ simulate nLeavesBigTree (head gs)
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