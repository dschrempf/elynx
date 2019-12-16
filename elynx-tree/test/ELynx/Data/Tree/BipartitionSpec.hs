{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Data.Tree.BipartitionSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Aug 30 09:38:50 2019.

-}

module ELynx.Data.Tree.BipartitionSpec
  (spec
  ) where

import qualified Data.ByteString.Lazy        as L
import qualified Data.Map                    as M
import           Data.Monoid
import qualified Data.Set                    as S
import           Data.Tree
import           Test.Hspec

import           ELynx.Data.Tree.Bipartition
import           ELynx.Data.Tree.Partition
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.InputOutput

pfrom :: [L.ByteString] -> Partition L.ByteString
pfrom = pfromset . S.fromList

treeFileSimple :: FilePath
treeFileSimple = "data/TreeDist.trees"

getSimpleTrees :: IO [Tree (PhyloLabel L.ByteString)]
getSimpleTrees = parseFileWith manyNewick treeFileSimple

bipartitionToBranchAnswer :: M.Map (Bipartition L.ByteString) (Sum Double)
bipartitionToBranchAnswer =
  M.fromList [ (bp (pfrom ["B"]) (pfrom ["A","C","D","E"]), Sum {getSum = 0.3})
             , (bp (pfrom ["B","C","D","E"]) (pfrom ["A"]), Sum {getSum = 0.1})
             , (bp (pfrom ["B","C","E"]) (pfrom ["A","D"]), Sum {getSum = 5.0e-2})
             , (bp (pfrom ["B","E"]) (pfrom ["A","C","D"]), Sum {getSum = 0.4})
             , (bp (pfrom ["C"]) (pfrom ["A","B","D","E"]), Sum {getSum = 1.0e-2})
             , (bp (pfrom ["D"]) (pfrom ["A","B","C","E"]), Sum {getSum = 0.25})
             , (bp (pfrom ["E"]) (pfrom ["A","B","C","D"]), Sum {getSum = 0.8}) ]

bipartitionsFirstTree :: S.Set (Bipartition L.ByteString)
bipartitionsFirstTree = S.fromList [ bp (pfrom ["B"]) (pfrom ["A","C","D","E"])
                                   , bp (pfrom ["B","C","D","E"]) (pfrom ["A"])
                                   , bp (pfrom ["B","D","E"]) (pfrom ["A","C"])
                                   , bp (pfrom ["B","E"]) (pfrom ["A","C","D"])
                                   , bp (pfrom ["C"]) (pfrom ["A","B","D","E"])
                                   , bp (pfrom ["D"]) (pfrom ["A","B","C","E"])
                                   , bp (pfrom ["E"]) (pfrom ["A","B","C","D"]) ]

bipartitionsSecondTree :: S.Set (Bipartition L.ByteString)
bipartitionsSecondTree = S.fromList [ bp (pfrom ["B"]) (pfrom ["A","C","D","E"])
                                    , bp (pfrom ["B","C","D","E"]) (pfrom ["A"])
                                    , bp (pfrom ["B","C","E"]) (pfrom ["A","D"])
                                    , bp (pfrom ["B","E"]) (pfrom ["A","C","D"])
                                    , bp (pfrom ["C"]) (pfrom ["A","B","D","E"])
                                    , bp (pfrom ["D"]) (pfrom ["A","B","C","E"])
                                    , bp (pfrom ["E"]) (pfrom ["A","B","C","D"])]

spec :: Spec
spec = do
  describe "bipartitions" $
    it "calculates correct bipartitions for sample trees" $ do
      simpleTrees <- map removeBrInfo <$> getSimpleTrees
      let t1 = head simpleTrees
          t2 = simpleTrees !! 1
      bipartitions t1 `shouldBe` bipartitionsFirstTree
      bipartitions t2 `shouldBe` bipartitionsSecondTree
  describe "bipartitionToBranch" $
    it "creates a map from bipartitions to branch lengths" $ do
      simpleTrees <- getSimpleTrees
      bipartitionToBranchLength label (Sum . brLen) (simpleTrees !! 2)
        `shouldBe` bipartitionToBranchAnswer
