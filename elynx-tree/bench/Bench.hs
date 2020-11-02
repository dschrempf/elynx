{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Bench
-- Description :  Various benchmarks
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Dec 16 13:33:27 2019.
module Main where

import Control.Parallel.Strategies
import Criterion.Main
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import ELynx.Tools hiding (Random)
import ELynx.Tree
import ELynx.Tree.Simulate.PointProcess
import System.Random.MWC

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO (Forest Phylo BS.ByteString)
getManyTrees = parseFileWith (someNewick Standard) treeFileMany

hugeTree :: IO (Tree BranchLength Int)
hugeTree = create >>= simulateReconstructedTree 50000 Random 1.0 0.9

sinN :: Int -> BranchLength -> BranchLength
sinN n x = iterate sin x !! n

func :: BranchLength -> BranchLength
func = sinN 200

hugeTreeCalcPar :: Int -> Tree BranchLength Int -> Tree BranchLength Int
hugeTreeCalcPar n t = first func t `using` parTree n

main :: IO ()
main = do
  !ts <- getManyTrees
  !ht <- first getLen <$> hugeTree
  let mr1 = hugeTreeCalcPar 0 ht
      mr2 = hugeTreeCalcPar 1 ht
  if mr1 == mr2
    then putStrLn "Map OK."
    else do
      print mr1
      print mr2
      error "Map wrong."
  let fr1 = (foldl' (+) 0 . branches) ht
      fr2 = parBranchFoldMap 1 id (+) ht
  if 1e-8 > abs (fr1 - fr2)
    then putStrLn "Fold OK."
    else do
      print fr1
      print fr2
      error "Fold wrong."
  defaultMain
    [ bgroup "bipartition" [bench "manyTrees" $ nf (map bipartitions) ts],
      bgroup
        "map strategies"
        [ bench "sequential" $ nf (hugeTreeCalcPar 0) ht,
          bench "parallel 3" $ nf (hugeTreeCalcPar 3) ht
        ],
      bgroup
        "fold strategies"
        [ bench "sequential" $ nf (parBranchFoldMap 0 func (+)) ht,
          bench "parallel 3" $ nf (parBranchFoldMap 3 func (+)) ht
        ]
    ]
