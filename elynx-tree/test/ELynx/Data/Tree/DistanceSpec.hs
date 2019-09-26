{-# LANGUAGE FlexibleInstances #-}
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
  (spec
  ) where

import qualified Data.ByteString.Lazy.Char8           as L
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Containers ()

import           ELynx.Data.Tree.Distance
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.Equality
import           ELynx.Tools.InputOutput

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
symmetricDistanceAnswers :: [Int]
symmetricDistanceAnswers =
  [ 6, 8, 0, 0, 12, 20, 18, 20, 10, 2, 10, 4, 4, 4, 4, 4, 10, 16, 8, 2, 4, 0, 0,
    0, 10, 4, 0, 0, 2, 2, 0, 0, 4, 0, 2, 0, 8, 6, 2, 6, 4, 4, 8, 0, 0, 4, 2, 0,
    10, 0, 0, 10 ]

branchScoreDistanceAnswers :: [Double]
branchScoreDistanceAnswers =
  [ 8.567916e-02, 9.570577e-02, 1.704571e-02, 7.603990e-03, 6.149761e-01,
    3.557070e-01, 2.329811e-01, 3.820208e-01, 1.895421e-02, 6.302364e-03,
    2.083286e-02, 1.023777e-03, 2.138244e-02, 1.444380e-02, 1.958628e-02,
    6.089461e-03, 2.551873e-02, 8.041220e-02, 4.123102e-02, 8.241811e-03,
    2.623805e-02, 2.109278e-02, 1.953769e-02, 4.459926e-03, 6.594537e-02,
    7.040703e-02, 8.603133e-03, 3.878009e-03, 2.969969e-02, 2.505262e-02,
    2.095988e-02, 8.461041e-03, 5.228005e-02, 6.001320e-02, 8.276652e-03,
    6.966115e-03, 7.701581e-02, 4.946339e-02, 2.548024e-02, 5.800598e-03,
    3.875927e-02, 2.836737e-02, 9.059706e-02, 1.333325e-02, 5.071356e-02,
    7.433056e-02, 3.854717e-02, 3.255993e-02, 1.581909e-01, 6.813096e-02,
    8.210513e-02, 7.664642e-02 ]

prop_dist_same_tree :: (Num b, Eq b) => (Tree a -> Tree a -> b) -> Tree a -> Bool
prop_dist_same_tree distanceMeasure t = distanceMeasure t t == 0

-- TODO: Microsporidia trees with branch support values.
-- getMicrospoPoissonTree :: IO (Tree (PhyloLabel L.ByteString))
-- getMicrospoPoissonTree = parseFileWith newick "data/MicrospoPoisson.tree"
-- getMicrospoUDM32Tree = parseFileWith newick "data/MicrospoEDM32.tree"
-- getMicrospoUDM64Tree = parseFileWith newick "data/MicrospoEDM64.tree"

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)

spec :: Spec
spec = do
  describe "symmetricDistance" $
    it "calculates correct distances for sample trees" $ do
      simpleTrees <- getSimpleTrees
      symmetricDistance (head simpleTrees) (simpleTrees !! 1) `shouldBe` 2
      manyTrees <- getManyTrees
      -- Since treedist computes the distance between adjacent pairs, in the
      -- following manner: [tr0, tr1, tr2, tr3] -> [dist tr0 tr1, dist tr2 tr3],
      -- we have to skip some distances.
      each 2 (computeAdjacentDistances (symmetricDistanceWith getName) manyTrees)
        `shouldBe` symmetricDistanceAnswers

  describe "incompatibleSplitDistance" $
    it "calculates correct distances for completely collapsed trees" $
    property $ prop_dist_same_tree
    (incompatibleSplitsDistance :: Tree (PhyloLabel Int) -> Tree (PhyloLabel Int) -> Int)
  describe "branchScoreDistance" $
    it "calculates correct distances for sample trees" $ do
      manyTrees <- getManyTrees
      print branchScoreDistanceAnswers
      each 2 (computeAdjacentDistances branchScoreDistance manyTrees)
        `shouldSatisfy` nearlyEqListWith 1e-5 branchScoreDistanceAnswers
