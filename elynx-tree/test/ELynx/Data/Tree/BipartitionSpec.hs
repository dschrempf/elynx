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

import qualified Data.ByteString.Lazy         as L
import qualified Data.Map                     as M
import           Data.Monoid
import qualified Data.Set                     as S
import           Data.Tree
import           Test.Hspec

import           ELynx.Data.Tree.Bipartition
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.InputOutput

treeFileSimple :: FilePath
treeFileSimple = "data/TreeDist.trees"

getSimpleTrees :: IO [Tree PhyloByteStringLabel]
getSimpleTrees = parseFileWith manyNewick treeFileSimple

bipartitionToBranchAnswer :: M.Map (Bipartition L.ByteString) (Sum Double)
bipartitionToBranchAnswer =
  M.fromList [ (bp (S.fromList ["B"]) (S.fromList ["A","C","D","E"]), Sum {getSum = 0.3})
             , (bp (S.fromList ["B","C","D","E"]) (S.fromList ["A"]), Sum {getSum = 0.1})
             , (bp (S.fromList ["B","C","E"]) (S.fromList ["A","D"]), Sum {getSum = 5.0e-2})
             , (bp (S.fromList ["B","E"]) (S.fromList ["A","C","D"]), Sum {getSum = 0.4})
             , (bp (S.fromList ["C"]) (S.fromList ["A","B","D","E"]), Sum {getSum = 1.0e-2})
             , (bp (S.fromList ["D"]) (S.fromList ["A","B","C","E"]), Sum {getSum = 0.25})
             , (bp (S.fromList ["E"]) (S.fromList ["A","B","C","D"]), Sum {getSum = 0.8}) ]

bipartitionsFirstTree :: S.Set (Bipartition L.ByteString)
bipartitionsFirstTree = S.fromList [ bp (S.fromList ["B"]) (S.fromList ["A","C","D","E"])
                                   , bp (S.fromList ["B","C","D","E"]) (S.fromList ["A"])
                                   , bp (S.fromList ["B","D","E"]) (S.fromList ["A","C"])
                                   , bp (S.fromList ["B","E"]) (S.fromList ["A","C","D"])
                                   , bp (S.fromList ["C"]) (S.fromList ["A","B","D","E"])
                                   , bp (S.fromList ["D"]) (S.fromList ["A","B","C","E"])
                                   , bp (S.fromList ["E"]) (S.fromList ["A","B","C","D"]) ]

bipartitionsSecondTree :: S.Set (Bipartition L.ByteString)
bipartitionsSecondTree = S.fromList [ bp (S.fromList ["B"]) (S.fromList ["A","C","D","E"])
                                    , bp (S.fromList ["B","C","D","E"]) (S.fromList ["A"])
                                    , bp (S.fromList ["B","C","E"]) (S.fromList ["A","D"])
                                    , bp (S.fromList ["B","E"]) (S.fromList ["A","C","D"])
                                    , bp (S.fromList ["C"]) (S.fromList ["A","B","D","E"])
                                    , bp (S.fromList ["D"]) (S.fromList ["A","B","C","E"])
                                    , bp (S.fromList ["E"]) (S.fromList ["A","B","C","D"])]

spec :: Spec
spec = do
  describe "bipartitions" $
    it "calculates correct bipartitions for sample trees" $ do
      simpleTrees <- map removeBrLen <$> getSimpleTrees
      let t1 = head simpleTrees
          t2 = simpleTrees !! 1
      bipartitions t1 `shouldBe` bipartitionsFirstTree
      bipartitions t2 `shouldBe` bipartitionsSecondTree
  describe "bipartitionToBranch" $
    it "creates a map from bipartitions to branch lengths" $ do
      simpleTrees <- getSimpleTrees
      bipartitionToBranch pLabel (Sum . pBrLen) (simpleTrees !! 2)
        `shouldBe` bipartitionToBranchAnswer
