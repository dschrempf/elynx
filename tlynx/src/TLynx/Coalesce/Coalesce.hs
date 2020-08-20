{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- TODO: MERGE WITH SIMULATE.

-- |
--   Description :  Simulate reconstructed trees using the coalescent process
--   Copyright   :  (c) Dominik Schrempf 2020
--   License     :  GPL-3.0-or-later
--
--   Maintainer  :  dominik.schrempf@gmail.com
--   Stability   :  unstable
--   Portability :  portable
--
-- Creation date: Tue Feb 3 17:00:00 2020.
module TLynx.Coalesce.Coalesce
  ( coalesce,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async.Lifted.Safe
  ( mapConcurrently,
  )
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ask)
import Control.Parallel.Strategies
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import ELynx.Data.Tree
import ELynx.Export.Tree.Newick (toNewick)
import ELynx.Simulate.Coalescent (simulate)
import ELynx.Tools
import System.Random.MWC (initialize)
import TLynx.Coalesce.Options
import TLynx.Simulate.Simulate (nSubSamples)

-- | Simulate phylogenetic trees.
coalesce :: ELynx CoalesceArguments ()
coalesce = do
  l <- local <$> ask
  let s = argsSumStat l
  logNewSection "Arguments"
  $(logInfo) $ T.pack $ reportCoalesceArguments l
  logNewSection "Simulation"
  c <- liftIO getNumCapabilities
  $(logInfo) $ T.pack $ "Number of used cores: " <> show c
  trs <- case argsRho l of
    Nothing -> simulateNTreesConcurrently
    Just _ -> simulateAndSubSampleNTreesConcurrently
  let ls =
        if s
          then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
          else parMap rpar toNewick (map measurableToPhyloTree trs)
  let res = BL.unlines ls
  out "simulated trees" res ".tree"

simulateNTreesConcurrently :: ELynx CoalesceArguments (Forest Length Int)
simulateNTreesConcurrently = do
  (CoalesceArguments nT nL _ _ (Fixed s)) <- local <$> ask
  c <- liftIO getNumCapabilities
  gs <- liftIO $ initialize s >>= \g -> splitGen c g
  let chunks = getChunks c nT
  trss <-
    liftIO $
      mapConcurrently
        (\(n, g) -> replicateM n $ simulate nL g)
        (zip chunks gs)
  return $ concat trss

simulateAndSubSampleNTreesConcurrently ::
  ELynx CoalesceArguments (Forest Length Int)
simulateAndSubSampleNTreesConcurrently = do
  (CoalesceArguments nT nL mR _ (Fixed s)) <- local <$> ask
  c <- liftIO getNumCapabilities
  let r =
        fromMaybe
          ( error
              "cimulateAndSubSampleNTreesConcurrently: no sampling probability given."
          )
          mR
  let nLeavesBigTree = (round $ fromIntegral nL / r) :: Int
  gs <- liftIO $ initialize s >>= \g -> splitGen c g
  let chunks = getChunks c nT
  tr <- liftIO $ simulate nLeavesBigTree (head gs)
  logNewSection $
    T.pack $
      "Simulate one big tree with "
        <> show nLeavesBigTree
        <> " leaves."
  -- Log the base tree.
  $(logInfo) $ LT.toStrict $ LT.decodeUtf8 $ toNewick $ measurableToPhyloTree tr
  logNewSection $
    T.pack $
      "Sub sample "
        <> show nT
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

-- | Pair of branch length with number of extant children.
type BrLnNChildren = (BranchLength, Int)

-- | Possible summary statistic of phylogenetic trees. A list of tuples
-- (BranchLength, NumberOfExtantChildrenBelowThisBranch).
type NChildSumStat = [BrLnNChildren]

-- | Format the summary statistics in the following form:
-- @
--    nLeaves1 branchLength1
--    nLeaves2 branchLength2
--    ....
formatNChildSumStat :: NChildSumStat -> BL.ByteString
formatNChildSumStat s =
  BB.toLazyByteString . mconcat $ map formatNChildSumStatLine s

formatNChildSumStatLine :: BrLnNChildren -> BB.Builder
formatNChildSumStatLine (l, n) =
  BB.intDec n <> BB.char8 ' ' <> BB.doubleDec l <> BB.char8 '\n'

-- | Compute NChilSumStat for a phylogenetic tree.
toNChildSumStat :: Measurable e => Tree e a -> NChildSumStat
toNChildSumStat (Node br _ []) = [(getLen br, 1)]
toNChildSumStat (Node br _ ts) = (getLen br, sumCh) : concat nChSS
  where
    nChSS = map toNChildSumStat ts
    sumCh = sum $ map (snd . head) nChSS

-- TODO: MERGE WITH SIMULATE.
