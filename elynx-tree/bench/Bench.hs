{- |
Module      :  Bench
Description :  Various benchmarks
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon Dec 16 13:33:27 2019.

-}

import           Criterion.Main
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Tree

import           ELynx.Data.Tree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO [Tree (PhyloLabel L.ByteString)]
getManyTrees = parseFileWith manyNewick treeFileMany

main :: IO ()
main = do
  ts <- getManyTrees
  defaultMain
    [bgroup "bipartition" [bench "manyTrees" $ whnf (map bipartitions) ts]]
