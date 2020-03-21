{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Data.Tree.DistanceSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Aug 30 09:38:50 2019.

-}

module ELynx.Data.Tree.DistanceSpec
  ( spec
  )
where

import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Containers
                                                ( )

import           ELynx.Data.Tree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools

treeFileSimple :: FilePath
treeFileSimple = "data/TreeDist.trees"

getSimpleTrees :: IO [Tree (PhyloLabel L.ByteString)]
getSimpleTrees = parseFileWith manyNewick treeFileSimple

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO [Tree (PhyloLabel L.ByteString)]
getManyTrees = parseFileWith manyNewick treeFileMany

-- I used treedist from Phylip to get the correct results.
-- See http://evolution.genetics.washington.edu/phylip/doc/treedist.html.
symmetricAnswers :: [Int]
symmetricAnswers =
  [ 6
  , 8
  , 0
  , 0
  , 12
  , 20
  , 18
  , 20
  , 10
  , 2
  , 10
  , 4
  , 4
  , 4
  , 4
  , 4
  , 10
  , 16
  , 8
  , 2
  , 4
  , 0
  , 0
  , 0
  , 10
  , 4
  , 0
  , 0
  , 2
  , 2
  , 0
  , 0
  , 4
  , 0
  , 2
  , 0
  , 8
  , 6
  , 2
  , 6
  , 4
  , 4
  , 8
  , 0
  , 0
  , 4
  , 2
  , 0
  , 10
  , 0
  , 0
  , 10
  ]

branchScoreAnswers :: [Double]
branchScoreAnswers =
  [ 8.567916e-02
  , 9.570577e-02
  , 1.704571e-02
  , 7.603990e-03
  , 6.149761e-01
  , 3.557070e-01
  , 2.329811e-01
  , 3.820208e-01
  , 1.895421e-02
  , 6.302364e-03
  , 2.083286e-02
  , 1.023777e-03
  , 2.138244e-02
  , 1.444380e-02
  , 1.958628e-02
  , 6.089461e-03
  , 2.551873e-02
  , 8.041220e-02
  , 4.123102e-02
  , 8.241811e-03
  , 2.623805e-02
  , 2.109278e-02
  , 1.953769e-02
  , 4.459926e-03
  , 6.594537e-02
  , 7.040703e-02
  , 8.603133e-03
  , 3.878009e-03
  , 2.969969e-02
  , 2.505262e-02
  , 2.095988e-02
  , 8.461041e-03
  , 5.228005e-02
  , 6.001320e-02
  , 8.276652e-03
  , 6.966115e-03
  , 7.701581e-02
  , 4.946339e-02
  , 2.548024e-02
  , 5.800598e-03
  , 3.875927e-02
  , 2.836737e-02
  , 9.059706e-02
  , 1.333325e-02
  , 5.071356e-02
  , 7.433056e-02
  , 3.854717e-02
  , 3.255993e-02
  , 1.581909e-01
  , 6.813096e-02
  , 8.210513e-02
  , 7.664642e-02
  ]

prop_dist_same_tree
  :: (Num b, Eq b) => (Tree a -> Tree a -> b) -> Tree a -> Bool
prop_dist_same_tree distanceMeasure t = distanceMeasure t t == 0

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)

multifurcating :: Tree Char
multifurcating = Node
  ' '
  [Node 'A' [], Node 'B' [], Node ' ' [Node 'C' [], Node 'D' [], Node 'E' []]]

bifurcatingComp :: Tree Char
bifurcatingComp = Node
  ' '
  [ Node ' ' [Node 'A' [], Node 'B' []]
  , Node ' ' [Node 'C' [], Node ' ' [Node 'D' [], Node 'E' []]]
  ]

bifurcatingIncomp :: Tree Char
bifurcatingIncomp = Node
  ' '
  [ Node ' ' [Node 'A' [], Node 'C' []]
  , Node ' ' [Node 'B' [], Node ' ' [Node 'D' [], Node 'E' []]]
  ]

incSplitTree1a :: Tree (PhyloLabel L.ByteString)
incSplitTree1a = parseByteStringWith "" oneNewickIqTree "((a,b)0.7,(c,d));"

incSplitTree1b :: Tree (PhyloLabel L.ByteString)
incSplitTree1b = parseByteStringWith "" oneNewickIqTree "((a,b)0.7,c,d);"

incSplitTree2 :: Tree (PhyloLabel L.ByteString)
incSplitTree2 = parseByteStringWith "" oneNewickIqTree "((a,c),(b,d));"

incSplitTree3 :: Tree (PhyloLabel L.ByteString)
incSplitTree3 = parseByteStringWith "" oneNewickIqTree "(((a,b)0.7,c),(d,e));"

incSplitTree4 :: Tree (PhyloLabel L.ByteString)
incSplitTree4 = parseByteStringWith "" oneNewickIqTree "(((a,c),b),(d,e));"


spec :: Spec
spec = do
  describe "symmetric" $ do
    it "calculates correct distances for sample trees" $ do
      simpleTrees <- getSimpleTrees
      symmetric (head simpleTrees) (simpleTrees !! 1) `shouldBe` 2
      manyTrees <- getManyTrees
      -- Since treedist computes the distance between adjacent pairs, in the
      -- following manner: [tr0, tr1, tr2, tr3] -> [dist tr0 tr1, dist tr2 tr3],
      -- we have to skip some distances.
      each 2 (adjacent (symmetricWith getName) manyTrees)
        `shouldBe` symmetricAnswers
    it "is zero for a collection of random trees"
      $ property
      $ prop_dist_same_tree
          (symmetric :: Tree (PhyloLabel Double)
            -> Tree (PhyloLabel Double)
            -> Int
          )

  describe "incompatibleSplit" $ do
    it "calculates correct distances for sample trees" $ do
      incompatibleSplits multifurcating bifurcatingComp `shouldBe` 0
      incompatibleSplits bifurcatingComp multifurcating `shouldBe` 0
      incompatibleSplits bifurcatingIncomp multifurcating `shouldBe` 2
      incompatibleSplits multifurcating bifurcatingIncomp `shouldBe` 2
    it "calculates correct distances for sample trees with branch support" $ do
      incompatibleSplits incSplitTree1a incSplitTree2 `shouldBe` 2
      incompatibleSplits incSplitTree1b incSplitTree2 `shouldBe` 2
      incompatibleSplits (collapse 0.71 incSplitTree1a) incSplitTree2
        `shouldBe` 2
      incompatibleSplits (collapse 0.71 incSplitTree1b) incSplitTree2
        `shouldBe` 0
      incompatibleSplits (collapse 0.71 incSplitTree3) incSplitTree4
        `shouldBe` 0
    it "is zero for a collection of random trees"
      $ property
      $ prop_dist_same_tree
          (incompatibleSplits :: Tree (PhyloLabel Double)
            -> Tree (PhyloLabel Double)
            -> Int
          )

  describe "branchScore" $ do
    it "calculates correct distances for sample trees" $ do
      manyTrees <- getManyTrees
      -- print branchScoreAnswers
      each 2 (adjacent branchScore manyTrees)
        `shouldSatisfy` nearlyEqListWith 1e-5 branchScoreAnswers
    it "is zero for a collection of random trees"
      $ property
      $ prop_dist_same_tree
          (branchScore :: Tree (PhyloLabel Double)
            -> Tree (PhyloLabel Double)
            -> Double
          )
