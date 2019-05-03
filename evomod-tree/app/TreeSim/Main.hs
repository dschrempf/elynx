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

TODO: lambda ~ mu.

Mon Feb 4 14:26:11 CET 2019: Adding sampling probability rho. See Article
(Stadler2009) Stadler, T. On incomplete sampling under birth–death models and
connections to the sampling-based coalescent Journal of Theoretical Biology,
Elsevier BV, 2009, 261, 58-66

-}

module Main where

import           Control.Concurrent           (getNumCapabilities, myThreadId,
                                               threadCapability)
import           Control.Concurrent.Async     (replicateConcurrently)
import           Control.Monad                (replicateM, unless, when)
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Word
import           Data.Tree
import           Data.Vector                  (fromList)
import           System.Random.MWC

import           ArgParseTreeSim

import           EvoMod.Data.Tree.PhyloTree   (PhyloIntLabel)
import           EvoMod.Data.Tree.SumStat     (formatNChildSumStat,
                                               toNChildSumStat)
import           EvoMod.Definitions
import           EvoMod.Export.Tree.Newick    (toNewickPhyloIntTree)
import           EvoMod.Simulate.PointProcess (simulateReconstructedTree)


newSection :: String -> String
newSection h = unlines
  [ ""
  , "-- " ++ h ]

-- TODO TODO: Log facility.

main :: IO ()
main = do
  args <- parseArgs
  let v = verbosity args
      q = quiet args
      s = sumStat args
  c <- getNumCapabilities
  unless q $ do
    -- p <- Sys.getProgName
    -- a <- Sys.getArgs
    hdr <- programHeader
    putStr hdr
    putStr $ newSection "Arguments"
    putStr $ reportArgs args
    putStr $ newSection "Simulation"
  when v $ putStrLn $ "Number of used cores: " ++ show c
  trs <- simulateNTreesConcurrently c args
  let ls = if s
           then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
           else parMap rpar toNewickPhyloIntTree trs
  L.putStr $ L.unlines ls

simulateNTreesConcurrently :: Int -> Args -> IO [Tree PhyloIntLabel]
simulateNTreesConcurrently c (Args t n h l m r _ v _ s) = do
  -- when (l <= 0) (error "Speciation rate has to be larger than zero.")
  -- when (m <= 0) (error "Extinction rate has to be larger than zero.")
  -- when ((r <= 0) || (r > 1)) (error "Sampling probability has to in (0,1].")
  let l' = l * r
      m' = m - l * (1.0 - r)
  trsCon <- replicateConcurrently c (simulateNTrees (t `div` c) n h l' m' v s)
  trsRem <- simulateNTrees (t `mod` c) n h l' m' v s
  return $ concat trsCon ++ trsRem

simulateNTrees :: Int -> Int -> Maybe Double -> Double -> Double -> Bool
               -> Maybe [Word32]
               -> IO [Tree PhyloIntLabel]
simulateNTrees t n mH l m v s
  | t <= 0 = return []
  | otherwise = do
      when v reportCapability
      g <- maybe createSystemRandom (initialize . fromList) s
      let f = simulateReconstructedTree n mH l m g
      replicateM t f

-- simulateNBranchLengthNChildrenConcurrently :: Int -> Args -> IO T.Text
-- simulateNBranchLengthNChildrenConcurrently c (Args t n h l m _ v _ s) = do
--   trsCon <- replicateConcurrently c (simulateNBranchLengthNChildren (t `div` c) n h l m v s)
--   trsRem <- simulateNBranchLengthNChildren (t `mod` c) n h l m v s
--   let trs = concat trsCon ++ trsRem
--       ls  = parMap rpar formatNChildSumStat trs
--   return $ T.unlines ls


-- simulateNBranchLengthNChildren :: Int -> Int -> Maybe Double -> Double -> Double -> Bool
--                                -> Maybe Int
--                                -> IO [[(Double, Int)]]
-- simulateNBranchLengthNChildren t n mH l m v s = do
--   when v reportCapability
--   g <- maybe createSystemRandom (initialize . singleton . fromIntegral) s
--   let f = case mH of
--             Nothing -> simulateBranchLengthNChildrenRandomHeight n l m g
--             Just h  -> simulateBranchLengthNChildren n h l m g
--   replicateM t f

reportCapability :: IO ()
reportCapability = do
  i <- myThreadId
  (c, _) <- threadCapability i
  putStrLn $ "Running on core: " ++ show c
