{- |
Module      :  EvoMod.Data.Tree.DistanceSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Aug 30 09:38:50 2019.

-}

module EvoMod.Data.Tree.DistanceSpec
  (spec
  ) where

import           Data.Tree
import           Test.Hspec

import           EvoMod.Data.Tree.Distance
import qualified EvoMod.Data.Tree.NamedTree as T
import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Import.Tree.Newick
import           EvoMod.Tools.InputOutput

treeFileSimple :: FilePath
treeFileSimple = "data/TreeDist.trees"

getSimpleTrees :: IO [Tree PhyloByteStringLabel]
getSimpleTrees = parseFileWith manyNewick treeFileSimple

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO [Tree PhyloByteStringLabel]
getManyTrees = parseFileWith manyNewick treeFileMany

-- I used treedist from Phylip to get the correct results.
-- See http://evolution.genetics.washington.edu/phylip/doc/treedist.html.
manyAnswers :: [Int]
manyAnswers = [ 6, 8, 0, 0, 12, 20, 18, 20, 10, 2, 10, 4, 4, 4, 4, 4, 10, 16, 8,
                2, 4, 0, 0, 0, 10, 4, 0, 0, 2, 2, 0, 0, 4, 0, 2, 0, 8, 6, 2, 6,
                4, 4, 8, 0, 0, 4, 2, 0, 10, 0, 0, 10 ]

-- getMicrospoPoissonTree :: IO (Tree PhyloByteStringLabel)
-- getMicrospoPoissonTree = parseFileWith newick "TODO"

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)

spec :: Spec
spec = do
  -- describe "bipartitions" $
  --   it "calculates correct bipartitions for sample trees" $ do
  --     simpleTrees <- map removeBrLen <$> getSimpleTrees
  --     let t1 = head simpleTrees
  --         t2 = simpleTrees !! 1
  --         b1 = bipartitions t1
  --         b2 = bipartitions t2
  --     print b1
  --     print b2
  describe "symmetricDistance" $
    it "calculates correct distances for sample trees" $ do
      simpleTrees <- getSimpleTrees
      symmetricDistance (head simpleTrees) (simpleTrees !! 1) `shouldBe` 2
      manyTrees <- getManyTrees
      -- Since treedist computes the distance between adjacent pairs, in the
      -- following manner: [tr0, tr1, tr2, tr3] -> [dist tr0 tr1, dist tr2 tr3],
      -- we have to skip some distances.
      each 2 (computeAdjacentDistances (symmetricDistanceWith T.name) manyTrees) `shouldBe` manyAnswers
  -- -- TODO.
  -- describe "incompatibleSplitDistance" $
  --   it "calculates correct distances for sample trees" $ do
  --     simpleTrees <- getSimpleTrees
  --     symmetricDistance (head simpleTrees) (simpleTrees !! 1) `shouldBe` 2
  --     manyTrees <- map removeBrLen <$> getManyTrees
  --     -- Since treedist computes the distance between adjacent pairs, in the
  --     -- following manner: [tr0, tr1, tr2, tr3] -> [dist tr0 tr1, dist tr2 tr3],
  --     -- we have to skip some distances.
  --     each 2 (computeAdjacentDistances symmetricDistance manyTrees) `shouldBe` manyAnswers
