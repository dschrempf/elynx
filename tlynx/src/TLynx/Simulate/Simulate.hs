{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

{- |
   Description :  Simulate reconstructed trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3.0-or-later

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

module TLynx.Simulate.Simulate
  ( simulate
  )
where
import           Control.Monad.Trans.Reader     ( ask )
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
import           System.Random.MWC              ( GenIO
                                                , initialize
                                                )

import           TLynx.Simulate.Options

import           ELynx.Data.Tree
import           ELynx.Export.Tree.Newick       ( toNewick )
import           ELynx.Simulate.PointProcess    ( TimeSpec
                                                , simulateNReconstructedTrees
                                                , simulateReconstructedTree
                                                )
import           ELynx.Tools

-- | Simulate phylogenetic trees.
simulate :: ELynx SimulateArguments ()
simulate = do
  l <- local <$> ask
  let
    SimulateArguments nTrees nLeaves tHeight mrca lambda mu rho subS sumS (Fixed s)
      = l
  -- error "simulate: seed not available; please contact maintainer."
  when (isNothing tHeight && mrca)
    $ error "Cannot condition on MRCA (-M) when height is not given (-H)."
  c <- liftIO getNumCapabilities
  logNewSection "Arguments"
  $(logInfo) $ T.pack $ reportSimulateArguments l
  logNewSection "Simulation"
  $(logInfo) $ T.pack $ "Number of used cores: " <> show c
  gs <- liftIO $ initialize s >>= \gen -> splitGen c gen
  let chunks   = getChunks c nTrees
      timeSpec = fmap (, mrca) tHeight
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
  let res = L.unlines ls
  out "simulated trees" res ".tree"

simulateNTreesConcurrently
  :: Int
  -> Double
  -> Double
  -> Double
  -> TimeSpec
  -> [Int]
  -> [GenIO]
  -> ELynx SimulateArguments [Tree (PhyloLabel Int)]
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
  -> ELynx SimulateArguments [Tree (PhyloLabel Int)]
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
