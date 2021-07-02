{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Bench
-- Description :  Various benchmarks
-- Copyright   :  (c) Dominik Schrempf 2021
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
import Data.Foldable
import ELynx.Tools hiding (Random)
import ELynx.Tree
import ELynx.Tree.Simulate.PointProcess
import Length
import Lens
import System.Random.MWC
import Tree

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO (Forest Phylo Name)
getManyTrees = parseFileWith (someNewick Standard) treeFileMany

hugeTree :: IO (Tree Length Int)
hugeTree = create >>= simulateReconstructedTree 50000 Random 1.0 0.9

sinN :: Int -> Length -> Length
sinN n x = iterate sin x !! n

func :: Length -> Length
func = sinN 200

hugeTreeCalcPar :: Int -> Tree Length Int -> Tree Length Int
hugeTreeCalcPar n t = first func t `using` parTree n

main :: IO ()
main = do
  !ts <- getManyTrees
  !ht <- first getLength <$> hugeTree
  let
    !pht = lengthToPhyloTree ht
    mr1 = hugeTreeCalcPar 0 ht
    mr2 = hugeTreeCalcPar 1 ht
  if mr1 == mr2
    then putStrLn "Map OK."
    else do
      print mr1
      print mr2
      error "Map wrong."
  let fr1 = (foldl' (+) 0 . branches) ht
      fr2 = parBranchFoldMap 3 id (+) ht
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
        ],
      -- Unsafe operations are fast, safe operations are roughly 50 percent slower.
      bgroup
        "length"
        [ bench "length sum foldl' safe" $ nf lengthSumFoldl' [0 .. 10000000],
          bench "length sum foldl' unsafe" $ nf lengthSumFoldl'Unsafe [0 .. 10000000],
          bench "length sum foldl' num instance" $ nf lengthSumFoldl'NumInstance [0 .. 10000000],
          bench "double sum foldl'" $ nf doubleSumFoldl' [0 .. 10000000],
          bench "double sum" $ nf doubleSum [0 .. 10000000]
        ],
      -- Lenses are fast.
      bgroup
        "lens"
        [ bench "sum with getter" $ nf sumWithGetter [0 .. 1000000 :: Length],
          bench "sum with accessor function" $ nf sumWithAccessorFunction [0 .. 1000000 :: Length],
          bench "sum with setter and getter" $ nf sumWithSetter [0 .. 1000000 :: Length],
          bench "sum with modify and accessor functions" $ nf sumWithModifyFunction [0 .. 1000000 :: Length]
        ],
      bgroup
        "traversable"
        [ bench "traverse tree" $ nf toLengthTreeTraversable pht,
          bench "traverse tree" $ nf toLengthTreeBitraversable pht
        ]
    ]

-- Results 2020 Nov 2, commit aee6818.

-- benchmarking bipartition/manyTrees
-- time                 9.631 ms   (9.583 ms .. 9.721 ms)
--                      0.998 R²   (0.995 R² .. 1.000 R²)
-- mean                 9.723 ms   (9.670 ms .. 9.879 ms)
-- std dev              220.5 μs   (87.68 μs .. 421.0 μs)

-- benchmarking map strategies/sequential
-- time                 851.4 ms   (666.8 ms .. 1.025 s)
--                      0.994 R²   (0.978 R² .. 1.000 R²)
-- mean                 735.3 ms   (676.0 ms .. 766.2 ms)
-- std dev              66.42 ms   (55.94 ms .. 69.47 ms)
-- variance introduced by outliers: 22% (moderately inflated)

-- benchmarking map strategies/parallel 3
-- time                 362.1 ms   (348.5 ms .. 377.0 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 354.6 ms   (348.1 ms .. 358.2 ms)
-- std dev              4.959 ms   (1.499 ms .. 6.727 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking fold strategies/sequential
-- time                 762.0 ms   (682.7 ms .. 913.8 ms)
--                      0.995 R²   (0.992 R² .. 1.000 R²)
-- mean                 676.3 ms   (647.5 ms .. 720.1 ms)
-- std dev              42.34 ms   (11.72 ms .. 57.27 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking fold strategies/parallel 3
-- time                 413.4 ms   (330.2 ms .. 536.7 ms)
--                      0.990 R²   (0.972 R² .. 1.000 R²)
-- mean                 368.3 ms   (345.6 ms .. 394.6 ms)
-- std dev              25.80 ms   (7.999 ms .. 35.08 ms)
-- variance introduced by outliers: 20% (moderately inflated)

-- benchmarking traversable/traverse tree
-- time                 119.3 ms   (116.5 ms .. 121.8 ms)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 123.4 ms   (121.7 ms .. 128.1 ms)
-- std dev              3.891 ms   (1.190 ms .. 6.197 ms)
-- variance introduced by outliers: 11% (moderately inflated)

-- benchmarking traversable/traverse tree
-- time                 123.6 ms   (121.4 ms .. 125.0 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 122.4 ms   (120.1 ms .. 123.7 ms)
-- std dev              2.432 ms   (1.358 ms .. 3.715 ms)
-- variance introduced by outliers: 11% (moderately inflated)
