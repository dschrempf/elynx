{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Data.Tree.BipartitionSpec
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Aug 30 09:38:50 2019.

-}

module ELynx.Data.Tree.BipartitionSpec
  ( spec
  )
where

import qualified Data.ByteString.Lazy          as L
import qualified Data.Map                      as M
import           Data.Monoid
import qualified Data.Set                      as S
import           Data.Tree
import           Test.Hspec

import           ELynx.Data.Tree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools

sfrom :: [L.ByteString] -> S.Set L.ByteString
sfrom = S.fromList

treeFileSimple :: FilePath
treeFileSimple = "data/TreeDist.trees"

getSimpleTrees :: IO [Tree (PhyloLabel L.ByteString)]
getSimpleTrees = parseFileWith (manyNewick Standard) treeFileSimple

bipartitionToBranchAnswer :: M.Map (Bipartition L.ByteString) (Sum Double)
bipartitionToBranchAnswer = M.fromList
  [ (bp (sfrom ["B"]) (sfrom ["A", "C", "D", "E"]), Sum { getSum = 0.3 })
  , (bp (sfrom ["B", "C", "D", "E"]) (sfrom ["A"]), Sum { getSum = 0.1 })
  , (bp (sfrom ["B", "C", "E"]) (sfrom ["A", "D"]), Sum { getSum = 5.0e-2 })
  , (bp (sfrom ["B", "E"]) (sfrom ["A", "C", "D"]), Sum { getSum = 0.4 })
  , (bp (sfrom ["C"]) (sfrom ["A", "B", "D", "E"]), Sum { getSum = 1.0e-2 })
  , (bp (sfrom ["D"]) (sfrom ["A", "B", "C", "E"]), Sum { getSum = 0.25 })
  , (bp (sfrom ["E"]) (sfrom ["A", "B", "C", "D"]), Sum { getSum = 0.8 })
  ]

bipartitionsFirstTree :: S.Set (Bipartition L.ByteString)
bipartitionsFirstTree = S.fromList
  [ bp (sfrom ["B"])                (sfrom ["A", "C", "D", "E"])
  , bp (sfrom ["B", "C", "D", "E"]) (sfrom ["A"])
  , bp (sfrom ["B", "D", "E"])      (sfrom ["A", "C"])
  , bp (sfrom ["B", "E"])           (sfrom ["A", "C", "D"])
  , bp (sfrom ["C"])                (sfrom ["A", "B", "D", "E"])
  , bp (sfrom ["D"])                (sfrom ["A", "B", "C", "E"])
  , bp (sfrom ["E"])                (sfrom ["A", "B", "C", "D"])
  ]

bipartitionsSecondTree :: S.Set (Bipartition L.ByteString)
bipartitionsSecondTree = S.fromList
  [ bp (sfrom ["B"])                (sfrom ["A", "C", "D", "E"])
  , bp (sfrom ["B", "C", "D", "E"]) (sfrom ["A"])
  , bp (sfrom ["B", "C", "E"])      (sfrom ["A", "D"])
  , bp (sfrom ["B", "E"])           (sfrom ["A", "C", "D"])
  , bp (sfrom ["C"])                (sfrom ["A", "B", "D", "E"])
  , bp (sfrom ["D"])                (sfrom ["A", "B", "C", "E"])
  , bp (sfrom ["E"])                (sfrom ["A", "B", "C", "D"])
  ]

spec :: Spec
spec = do
  describe "bipartitions"
    $ it "calculates correct bipartitions for sample trees"
    $ do
        simpleTrees <- map removeBrInfo <$> getSimpleTrees
        let t1 = head simpleTrees
            t2 = simpleTrees !! 1
        bipartitions t1 `shouldBe` bipartitionsFirstTree
        bipartitions t2 `shouldBe` bipartitionsSecondTree
  describe "bipartitionToBranch"
    $ it "creates a map from bipartitions to branch lengths"
    $ do
        simpleTrees <- getSimpleTrees
        bipartitionToBranchLength label (Sum . getLen) (simpleTrees !! 2)
          `shouldBe` bipartitionToBranchAnswer
