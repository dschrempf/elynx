{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.BipartitionSpec
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 30 09:38:50 2019.
module ELynx.Tree.BipartitionSpec
  ( spec,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import ELynx.Tools
import ELynx.Tree
import Test.Hspec

sfrom :: [BS.ByteString] -> S.Set BS.ByteString
sfrom = S.fromList

treeFileSimple :: FilePath
treeFileSimple = "data/TreeDist.trees"

getSimpleTrees :: IO (Forest Phylo BS.ByteString)
getSimpleTrees = parseFileWith (someNewick Standard) treeFileSimple

bipartitionToBranchAnswer :: Map (Bipartition BS.ByteString) Length
bipartitionToBranchAnswer =
  M.fromList
    [ (bpUnsafe (sfrom ["B"]) (sfrom ["A", "C", "D", "E"]), 0.3),
      (bpUnsafe (sfrom ["B", "C", "D", "E"]) (sfrom ["A"]), 0.1),
      (bpUnsafe (sfrom ["B", "C", "E"]) (sfrom ["A", "D"]), 5.0e-2),
      (bpUnsafe (sfrom ["B", "E"]) (sfrom ["A", "C", "D"]), 0.4),
      (bpUnsafe (sfrom ["C"]) (sfrom ["A", "B", "D", "E"]), 1.0e-2),
      (bpUnsafe (sfrom ["D"]) (sfrom ["A", "B", "C", "E"]), 0.25),
      (bpUnsafe (sfrom ["E"]) (sfrom ["A", "B", "C", "D"]), 0.8)
    ]

bipartitionsFirstTree :: Set (Bipartition BS.ByteString)
bipartitionsFirstTree =
  S.fromList
    [ bpUnsafe (sfrom ["B"]) (sfrom ["A", "C", "D", "E"]),
      bpUnsafe (sfrom ["B", "C", "D", "E"]) (sfrom ["A"]),
      bpUnsafe (sfrom ["B", "D", "E"]) (sfrom ["A", "C"]),
      bpUnsafe (sfrom ["B", "E"]) (sfrom ["A", "C", "D"]),
      bpUnsafe (sfrom ["C"]) (sfrom ["A", "B", "D", "E"]),
      bpUnsafe (sfrom ["D"]) (sfrom ["A", "B", "C", "E"]),
      bpUnsafe (sfrom ["E"]) (sfrom ["A", "B", "C", "D"])
    ]

bipartitionsSecondTree :: Set (Bipartition BS.ByteString)
bipartitionsSecondTree =
  S.fromList
    [ bpUnsafe (sfrom ["B"]) (sfrom ["A", "C", "D", "E"]),
      bpUnsafe (sfrom ["B", "C", "D", "E"]) (sfrom ["A"]),
      bpUnsafe (sfrom ["B", "C", "E"]) (sfrom ["A", "D"]),
      bpUnsafe (sfrom ["B", "E"]) (sfrom ["A", "C", "D"]),
      bpUnsafe (sfrom ["C"]) (sfrom ["A", "B", "D", "E"]),
      bpUnsafe (sfrom ["D"]) (sfrom ["A", "B", "C", "E"]),
      bpUnsafe (sfrom ["E"]) (sfrom ["A", "B", "C", "D"])
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
