{-# LANGUAGE TupleSections #-}

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

module Main where

import           Control.Concurrent                   (getNumCapabilities)
import           Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8           as L
import           Data.Maybe
import qualified Data.Sequence                        as Seq
import           Data.Tree
import           System.IO

import           OptionsTreeSim

import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.PhyloTree           (PhyloIntLabel)
import           EvoMod.Data.Tree.SumStat             (formatNChildSumStat,
                                                       toNChildSumStat)
import           EvoMod.Data.Tree.Tree
import           EvoMod.Export.Tree.Newick            (toNewick)
import           EvoMod.Simulate.PointProcess         (simulateNReconstructedTrees,
                                                       simulateReconstructedTree)
import           EvoMod.Tools.Concurrent
import           EvoMod.Tools.Logger
import           EvoMod.Tools.Options

main :: IO ()
main = do
  a <- parseArgs
  h <- setupLogger (argsVerbosity a) (argsOutFileBaseName a)
  runReaderT simulate (Params a h)
  closeLogger h

simulate :: Simulation ()
simulate = do
  a <- arguments <$> ask
  when (isNothing (argsHeight a) && argsConditionMRCA a) $
    error "main: cannot condition on MRCA (-M) when height is not given (-H)."
  let s = argsSumStat a
  c <- lift getNumCapabilities
  lift programHeader >>= logS
  logNewSection "Arguments"
  logS $ reportArgs a
  logNewSection "Simulation"
  logS $ "Number of used cores: " ++ show c
  trs <- if argsSubSample a
         then simulateAndSubSampleNTreesConcurrently c a
         else simulateNTreesConcurrently c a
  let ls = if s
           then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
           else parMap rpar toNewick trs
  let mfn = argsOutFileBaseName a
  case mfn of
    Nothing -> logLBSQuiet $ L.unlines ls
    Just fn -> do
      let fn' = fn ++ ".tree"
      lift $ L.writeFile fn' $ L.unlines ls
      logS $ "Results written to file '" ++ fn' ++ "'."

simulateNTreesConcurrently :: Int -> Args -> Simulation [Tree PhyloIntLabel]
simulateNTreesConcurrently c (Args nT nL h cM l m r _ _ _ _ s) = do
  let l' = l * r
      m' = m - l * (1.0 - r)
  gs <- lift $ getNGen c s
  let chunks = getChunks c nT
      timeSpec = fmap (, cM) h
  trss <- mapConcurrently (\(n, g) -> simulateNReconstructedTrees n nL timeSpec l' m' g) (zip chunks gs)
  return $ concat trss

simulateAndSubSampleNTreesConcurrently :: Int -> Args -> Simulation [Tree PhyloIntLabel]
simulateAndSubSampleNTreesConcurrently c (Args nT nL h cM l m r _ _ _ _ s) = do
  let nLeavesBigTree = (round $ fromIntegral nL / r) :: Int
  gs <- lift $ getNGen c s
  let chunks = getChunks c nT
      timeSpec = fmap (, cM) h
  tr <- simulateReconstructedTree nLeavesBigTree timeSpec l m (head gs)
  logNewSection $ "Simulate one big tree with " ++ show nLeavesBigTree ++ " leaves."
  logLBS $ toNewick tr
  logNewSection $ "Sub sample " ++ show nT ++ " trees with " ++ show nL ++ " leaves."
  let lvs = Seq.fromList $ leaves tr
  trss <- mapConcurrently (\(nSamples, g) -> nSubSamples nSamples lvs nL tr g) (zip chunks gs)
  let trs = catMaybes $ concat trss
  return $ map prune trs

type Simulation = ReaderT Params IO

data Params = Params { arguments  :: Args
                     , mLogHandle :: Maybe Handle }

instance Logger Params where
  verbosity = argsVerbosity . arguments
  mHandle = mLogHandle
