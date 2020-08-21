{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
--   Description :  Simulate reconstructed trees
--   Copyright   :  (c) Dominik Schrempf 2018
--   License     :  GPL-3.0-or-later
--
--   Maintainer  :  dominik.schrempf@gmail.com
--   Stability   :  unstable
--   Portability :  portable
--
-- Creation date: Tue Feb 27 17:27:16 2018.
--
-- See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
-- Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.
--
-- Mon Feb 4 14:26:11 CET 2019: Adding sampling probability rho. See Article
-- (Stadler2009) Stadler, T. On incomplete sampling under birth–death models and
-- connections to the sampling-based coalescent Journal of Theoretical Biology,
-- Elsevier BV, 2009, 261, 58-66
module TLynx.Simulate.Simulate
  ( simulate,
    nSubSamples,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async.Lifted.Safe
  ( mapConcurrently,
  )
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader hiding (local)
import Control.Parallel.Strategies
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import ELynx.Data.Tree
import ELynx.Export.Tree.Newick
import qualified ELynx.Simulate.Coalescent as CS
import qualified ELynx.Simulate.PointProcess as PP
import ELynx.Tools
import System.Random.MWC
import TLynx.Simulate.Options

-- | Simulate phylogenetic trees using birth and death process.
simulate :: ELynx SimulateArguments ()
simulate = do
  l@(SimulateArguments nTrees nLeaves pr subS sumS (Fixed s)) <- local <$> ask
  c <- liftIO getNumCapabilities
  logNewSection "Arguments"
  $(logInfo) $ T.pack $ reportSimulateArguments l
  logNewSection "Simulation"
  $(logInfo) $ T.pack $ "Number of used cores: " <> show c
  gs <- liftIO $ initialize s >>= \gen -> splitGen c gen
  let chunks = getChunks c nTrees
  trs <- case pr of
    (BirthDeath lambda mu mRho mHeight) -> do
      let rho = fromMaybe 1.0 mRho
          -- This is bad code, but I don't want to change the definition of 'TimeSpec'.
          timeSpec = case mHeight of
            Nothing -> Nothing
            Just (Mrca h) -> Just (h, True)
            Just (Origin h) -> Just (h, False)
      case subS of
        Nothing -> liftIO $ bdSimulateNTreesConcurrently nLeaves lambda mu rho timeSpec chunks gs
        Just p ->
          bdSimulateAndSubSampleNTreesConcurrently
            nLeaves
            lambda
            mu
            rho
            p
            timeSpec
            chunks
            gs
    Coalescent -> case subS of
      Nothing -> liftIO $ coalSimulateNTreesConcurrently nLeaves chunks gs
      Just p -> coalSimulateAndSubSampleNTreesConcurrently nLeaves p chunks gs
  let ls =
        if sumS
          then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
          else parMap rpar toNewick $ map measurableToPhyloTree trs
  let res = BL.unlines ls
  out "simulated trees" res ".tree"

bdSimulateNTreesConcurrently ::
  Int ->
  Double ->
  Double ->
  Double ->
  PP.TimeSpec ->
  [Int] ->
  [GenIO] ->
  IO (Forest Length Int)
bdSimulateNTreesConcurrently nLeaves l m r timeSpec chunks gs = do
  let l' = l * r
      m' = m - l * (1.0 - r)
  trss <-
    mapConcurrently
      (\(n, g) -> PP.simulateNReconstructedTrees n nLeaves timeSpec l' m' g)
      (zip chunks gs)
  return $ concat trss

coalSimulateNTreesConcurrently ::
  Int ->
  [Int] ->
  [GenIO] ->
  IO (Forest Length Int)
coalSimulateNTreesConcurrently nL chunks gs = do
  trss <-
      mapConcurrently
        (\(n, g) -> replicateM n $ CS.simulate nL g)
        (zip chunks gs)
  return $ concat trss

bdSimulateAndSubSampleNTreesConcurrently ::
  Int ->
  Double ->
  Double ->
  Double ->
  Double ->
  PP.TimeSpec ->
  [Int] ->
  [GenIO] ->
  ELynx SimulateArguments (Forest Length Int)
bdSimulateAndSubSampleNTreesConcurrently nLeaves l m r p timeSpec chunks gs = do
  let nLeavesBigTree = (round $ fromIntegral nLeaves / p) :: Int
      l' = l * r
      m' = m - l * (1.0 - r)
  logNewSection $
    T.pack $
      "Simulate one big tree with "
        <> show nLeavesBigTree
        <> " leaves."
  tr <- liftIO $ PP.simulateReconstructedTree nLeavesBigTree timeSpec l' m' (head gs)
  -- Log the base tree.
  $(logInfo) $ LT.toStrict $ LT.decodeUtf8 $ toNewick $ measurableToPhyloTree tr
  logNewSection $
    T.pack $
      "Sub sample "
        <> show (sum chunks)
        <> " trees with "
        <> show nLeaves
        <> " leaves."
  let lvs = Seq.fromList $ leaves tr
  trss <-
    liftIO $
      mapConcurrently
        (\(nSamples, g) -> nSubSamples nSamples lvs nLeaves tr g)
        (zip chunks gs)
  let trs = catMaybes $ concat trss
  return $ map prune trs

coalSimulateAndSubSampleNTreesConcurrently ::
  Int ->
  Double ->
  [Int] ->
  [GenIO] ->
  ELynx SimulateArguments (Forest Length Int)
coalSimulateAndSubSampleNTreesConcurrently nL p chunks gs = do
  let nLeavesBigTree = (round $ fromIntegral nL / p) :: Int
  logNewSection $
    T.pack $
      "Simulate one big tree with "
        <> show nLeavesBigTree
        <> " leaves."
  tr <- liftIO $ CS.simulate nLeavesBigTree (head gs)
  -- Log the base tree.
  $(logInfo) $ LT.toStrict $ LT.decodeUtf8 $ toNewick $ measurableToPhyloTree tr
  logNewSection $
    T.pack $
      "Sub sample "
        <> show (sum chunks)
        <> " trees with "
        <> show nL
        <> " leaves."
  let lvs = Seq.fromList $ leaves tr
  trss <-
    liftIO $
      mapConcurrently
        (\(nSamples, g) -> nSubSamples nSamples lvs nL tr g)
        (zip chunks gs)
  let trs = catMaybes $ concat trss
  return $ map prune trs

-- Extract a random subtree with @N@ leaves of a tree with @M@ leaves, where
-- @M>N@ (otherwise error). The complete list of leaves (names are assumed to be
-- unique) has to be provided as a 'Seq.Seq', and a 'Seq.Set', so that fast
-- sub-sampling as well as lookup are fast and so that these data structures do
-- not have to be recomputed when many sub-samples are requested.
nSubSamples ::
  Ord a =>
  Int ->
  Seq.Seq a ->
  Int ->
  Tree e a ->
  GenIO ->
  IO [Maybe (Tree e a)]
nSubSamples m lvs n tree g
  | Seq.length lvs < n =
    error
      "Given list of leaves is shorter than requested number of leaves."
  | otherwise = do
    lss <- grabble (toList lvs) m n g
    let lsSets = map Set.fromList lss
    return [dropLeavesWith (`Set.notMember` ls) tree | ls <- lsSets]

-- Pair of branch length with number of extant children.
type BrLnNChildren = (BranchLength, Int)

-- Possible summary statistic of phylogenetic trees. A list of tuples
-- (BranchLength, NumberOfExtantChildrenBelowThisBranch).
type NChildSumStat = [BrLnNChildren]

-- Format the summary statistics in the following form:
--
-- @
--   nLeaves1 branchLength1
--   nLeaves2 branchLength2
--   ....
-- @
formatNChildSumStat :: NChildSumStat -> BL.ByteString
formatNChildSumStat s =
  BB.toLazyByteString . mconcat $ map formatNChildSumStatLine s

formatNChildSumStatLine :: BrLnNChildren -> BB.Builder
formatNChildSumStatLine (l, n) =
  BB.intDec n <> BB.char8 ' ' <> BB.doubleDec l <> BB.char8 '\n'

-- Compute NChilSumStat for a phylogenetic tree.
toNChildSumStat :: Measurable e => Tree e a -> NChildSumStat
toNChildSumStat (Node br _ []) = [(getLen br, 1)]
toNChildSumStat (Node br _ ts) = (getLen br, sumCh) : concat nChSS
  where
    nChSS = map toNChildSumStat ts
    sumCh = sum $ map (snd . head) nChSS
