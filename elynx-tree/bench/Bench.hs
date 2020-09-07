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

import Criterion.Main
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import ELynx.Tools hiding (Random)
import ELynx.Tree
import ELynx.Tree.Simulate.PointProcess
import System.Random.MWC

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO (Forest Phylo BS.ByteString)
getManyTrees = parseFileWith (someNewick Standard) treeFileMany

hugeTree :: IO (Tree Length Int)
hugeTree = create >>= simulateReconstructedTree 1000 Random 1.0 0.9

sinN :: Int -> Double -> Double
sinN n x = iterate sin x !! n

hugeTreeCalc :: Tree Length Int -> Tree Double Int
hugeTreeCalc = first (sinN 200 . getLen)

hugeTreeCalcPar :: Tree Length Int -> Tree Double Int
hugeTreeCalcPar t = hugeTreeCalc t `using` parTree

hugeTreeCalcPar2 :: Tree Length Int -> Tree Double Int
hugeTreeCalcPar2 t = hugeTreeCalc t `using` parTree2

hugeTreeCalcPar3 :: Tree Length Int -> Tree Double Int
hugeTreeCalcPar3 t = hugeTreeCalc t `using` parTree3

main :: IO ()
main = do
  !ts <- getManyTrees
  !ht <- hugeTree
  defaultMain
    [ bgroup "bipartition" [bench "manyTrees" $ nf (map bipartitions) ts],
      bgroup
        "strategies"
        [ bench "exp, sequential" $ nf hugeTreeCalc ht,
          bench "exp, parallel 1" $ nf hugeTreeCalcPar ht,
          bench "exp, parallel 2" $ nf hugeTreeCalcPar2 ht,
          bench "exp, parallel 3" $ nf hugeTreeCalcPar3 ht
        ]
    ]
