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
import Data.ByteString.Lazy.Char8 (ByteString)
import ELynx.Data.Tree
import ELynx.Import.Tree.Newick
import ELynx.Tools

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO (Forest Phylo ByteString)
getManyTrees = parseFileWith (someNewick Standard) treeFileMany

main :: IO ()
main = do
  ts <- getManyTrees
  defaultMain
    [bgroup "bipartition" [bench "manyTrees" $ nf (map bipartitions) ts]]
