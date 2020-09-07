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
import Data.Foldable
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
hugeTree = create >>= simulateReconstructedTree 20000 Random 1.0 0.9

sinN :: Int -> Double -> Double
sinN n x = iterate sin x !! n

hugeTreeCalc :: Tree Length Int -> Tree Double Int
hugeTreeCalc = first (sinN 200 . getLen)

hugeTreeCalcPar :: Int -> Tree Length Int -> Tree Double Int
hugeTreeCalcPar n t = hugeTreeCalc t `using` parTree n

main :: IO ()
main = do
  !ts <- getManyTrees
  !ht <- hugeTree
  defaultMain
    [ bgroup "bipartition" [bench "manyTrees" $ nf (map bipartitions) ts],
      bgroup
        "strategies"
        [ bench "map sequential" $ nf hugeTreeCalc ht,
          bench "map parallel 1" $ nf (hugeTreeCalcPar 1) ht,
          bench "fld sequential" $ nf (foldl' (*) 1 . branches) ht,
          bench "fld parallel 1" $ nf (parBranchFold 1 (*)) ht
        ]
    ]
