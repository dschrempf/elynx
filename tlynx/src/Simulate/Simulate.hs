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
import           Control.Concurrent             ( getNumCapabilities )
import           Control.Concurrent.Async.Lifted.Safe
                                                ( mapConcurrently )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Maybe
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT
import           Data.Tree
import           System.Random.MWC              ( GenIO )

import           Simulate.Options

import           ELynx.Data.Tree.MeasurableTree ( prune )
import           ELynx.Data.Tree.PhyloTree      ( PhyloLabel )
import           ELynx.Data.Tree.SubSample      ( nSubSamples )
import           ELynx.Data.Tree.SumStat        ( formatNChildSumStat
                                                , toNChildSumStat
                                                )
import           ELynx.Data.Tree.Tree
import           ELynx.Export.Tree.Newick       ( toNewick )
import           ELynx.Simulate.PointProcess    ( TimeSpec
                                                , simulateNReconstructedTrees
                                                , simulateReconstructedTree
                                                )
import           ELynx.Tools.Concurrent
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Logger
import           ELynx.Tools.Reproduction       ( ELynx
                                                , getOutFilePath
                                                )

-- | Simulate phylogenetic trees.
simulate :: SimulateArguments -> ELynx ()
simulate a@(SimulateArguments nTrees nLeaves height mrca lambda mu rho subS sumS seed)
  = do
    when (isNothing height && mrca)
      $ error "Cannot condition on MRCA (-M) when height is not given (-H)."
    c <- liftIO getNumCapabilities
    logNewSection "Arguments"
    $(logInfo) $ T.pack $ reportSimulateArguments a
    logNewSection "Simulation"
    $(logInfo) $ T.pack $ "Number of used cores: " <> show c
    gs <- liftIO $ getNGen c seed
    let chunks   = getChunks c nTrees
        timeSpec = fmap (, mrca) height
    trs <- if subS
      then simulateAndSubSampleNTreesConcurrently nLeaves
                                                  lambda
                                                  mu
                                                  rho
                                                  timeSpec
                                                  chunks
                                                  gs
      else simulateNTreesConcurrently nLeaves lambda mu rho timeSpec chunks gs
    let ls = if sumS
          then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
          else parMap rpar toNewick trs
    fn <- getOutFilePath ".tree"
    let res = L.unlines ls
    out "simulated trees" res fn

simulateNTreesConcurrently
  :: Int
  -> Double
  -> Double
  -> Double
  -> TimeSpec
  -> [Int]
  -> [GenIO]
  -> ELynx [Tree (PhyloLabel Int)]
simulateNTreesConcurrently nLeaves l m r timeSpec chunks gs = do
  let l' = l * r
      m' = m - l * (1.0 - r)
  trss <- liftIO $ mapConcurrently
    (\(n, g) -> simulateNReconstructedTrees n nLeaves timeSpec l' m' g)
    (zip chunks gs)
  return $ concat trss

simulateAndSubSampleNTreesConcurrently
  :: Int
  -> Double
  -> Double
  -> Double
  -> TimeSpec
  -> [Int]
  -> [GenIO]
  -> ELynx [Tree (PhyloLabel Int)]
simulateAndSubSampleNTreesConcurrently nLeaves l m r timeSpec chunks gs = do
  let nLeavesBigTree = (round $ fromIntegral nLeaves / r) :: Int
  logNewSection
    $  T.pack
    $  "Simulate one big tree with "
    <> show nLeavesBigTree
    <> " leaves."
  tr <- liftIO $ simulateReconstructedTree nLeavesBigTree timeSpec l m (head gs)
  -- Log the base tree.
  $(logInfo) $ LT.toStrict $ LT.decodeUtf8 $ toNewick tr
  logNewSection
    $  T.pack
    $  "Sub sample "
    <> show (sum chunks)
    <> " trees with "
    <> show nLeaves
    <> " leaves."
  let lvs = Seq.fromList $ leaves tr
  trss <- liftIO $ mapConcurrently
    (\(nSamples, g) -> nSubSamples nSamples lvs nLeaves tr g)
    (zip chunks gs)
  let trs = catMaybes $ concat trss
  return $ map prune trs
