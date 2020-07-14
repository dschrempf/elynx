{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Data.Tree.BipartitionSpec
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 30 09:38:50 2019.
module ELynx.Data.Tree.BipartitionSpec
  ( spec,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import ELynx.Data.Tree
import ELynx.Import.Tree.Newick
import ELynx.Tools
import Test.Hspec

sfrom :: [ByteString] -> S.Set ByteString
sfrom = S.fromList

treeFileSimple :: FilePath
treeFileSimple = "data/TreeDist.trees"

getSimpleTrees :: IO (Forest Phylo ByteString)
getSimpleTrees = parseFileWith (manyNewick Standard) treeFileSimple

bipartitionToBranchAnswer :: Map (Bipartition ByteString) Length
bipartitionToBranchAnswer =
  M.fromList
    [ (bp (sfrom ["B"]) (sfrom ["A", "C", "D", "E"]), 0.3),
      (bp (sfrom ["B", "C", "D", "E"]) (sfrom ["A"]), 0.1),
      (bp (sfrom ["B", "C", "E"]) (sfrom ["A", "D"]), 5.0e-2),
      (bp (sfrom ["B", "E"]) (sfrom ["A", "C", "D"]), 0.4),
      (bp (sfrom ["C"]) (sfrom ["A", "B", "D", "E"]), 1.0e-2),
      (bp (sfrom ["D"]) (sfrom ["A", "B", "C", "E"]), 0.25),
      (bp (sfrom ["E"]) (sfrom ["A", "B", "C", "D"]), 0.8)
    ]

bipartitionsFirstTree :: Set (Bipartition ByteString)
bipartitionsFirstTree =
  S.fromList
    [ bp (sfrom ["B"]) (sfrom ["A", "C", "D", "E"]),
      bp (sfrom ["B", "C", "D", "E"]) (sfrom ["A"]),
      bp (sfrom ["B", "D", "E"]) (sfrom ["A", "C"]),
      bp (sfrom ["B", "E"]) (sfrom ["A", "C", "D"]),
      bp (sfrom ["C"]) (sfrom ["A", "B", "D", "E"]),
      bp (sfrom ["D"]) (sfrom ["A", "B", "C", "E"]),
      bp (sfrom ["E"]) (sfrom ["A", "B", "C", "D"])
    ]

bipartitionsSecondTree :: Set (Bipartition ByteString)
bipartitionsSecondTree =
  S.fromList
    [ bp (sfrom ["B"]) (sfrom ["A", "C", "D", "E"]),
      bp (sfrom ["B", "C", "D", "E"]) (sfrom ["A"]),
      bp (sfrom ["B", "C", "E"]) (sfrom ["A", "D"]),
      bp (sfrom ["B", "E"]) (sfrom ["A", "C", "D"]),
      bp (sfrom ["C"]) (sfrom ["A", "B", "D", "E"]),
      bp (sfrom ["D"]) (sfrom ["A", "B", "C", "E"]),
      bp (sfrom ["E"]) (sfrom ["A", "B", "C", "D"])
    ]

spec :: Spec
spec = do
  describe "bipartitions" $
    it "calculates correct bipartitions for sample trees" $
      do
        simpleTrees <- getSimpleTrees
        let t1 = head simpleTrees
            t2 = simpleTrees !! 1
        bipartitions t1 `shouldBe` Right bipartitionsFirstTree
        bipartitions t2 `shouldBe` Right bipartitionsSecondTree
  describe "bipartitionToBranch" $
    it "creates a map from bipartitions to branch lengths" $
      do
        simpleTrees <- getSimpleTrees
        (phyloToLengthTree (simpleTrees !! 2) >>= bipartitionToBranch)
          `shouldBe` Right bipartitionToBranchAnswer
