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
  -- print $ hugeTreeCalc ht == hugeTreeCalcPar 1 ht
  -- print $ (foldl' (+) 0 . branches) ht
  -- print $ parBranchFoldMap 1 id (+) ht
  defaultMain
    [ bgroup "bipartition" [bench "manyTrees" $ nf (map bipartitions) ts],
      bgroup
        "strategies"
        [ bench "map sequential" $ nf hugeTreeCalc ht,
          bench "map parallel 1" $ nf (hugeTreeCalcPar 1) ht,
          bench "fld sequential" $ nf (foldl' (+) 0 . branches) ht,
          bench "fld parallel 1" $ nf (parBranchFoldMap 1 id (+)) ht
        ]
    ]
