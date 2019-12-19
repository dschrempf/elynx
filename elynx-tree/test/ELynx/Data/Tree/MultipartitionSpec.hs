{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Data.Tree.MultipartitionSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Aug 30 09:38:50 2019.

-}

module ELynx.Data.Tree.MultipartitionSpec
  (spec
  ) where

import           Data.Set                             (Set, empty, fromList)
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Containers ()

import           ELynx.Data.Tree.BranchSupportTree
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.Multipartition       (Multipartition, mp,
                                                       multipartitions)
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Data.Tree.Subset               (sfromlist)

ex1 :: Tree Int
ex1 = Node 0 [Node 1 [], Node 2 [Node 4 [], Node 5 [], Node 6 []], Node 3 []]

sol1 :: Set (Multipartition Int)
sol1 = fromList [ mp [sfromlist [1], sfromlist [3], sfromlist [4,5,6]]
                , mp [sfromlist [1,3], sfromlist [4], sfromlist [5], sfromlist [6]] ]

ex2 :: Tree Int
ex2 = Node 0 [Node 1 [], Node 2 [], Node 0 [Node 3 [], Node 4 []], Node 5 []]

sol2 :: Set (Multipartition Int)
sol2 = fromList [ mp [sfromlist [1], sfromlist [2], sfromlist [3,4], sfromlist [5]] ]

prop_bifurcating_tree :: (Ord a, Measurable a, Named a, BranchSupportLabel a) => Tree a -> Bool
prop_bifurcating_tree t = multipartitions (removeMultifurcations t) == empty

spec :: Spec
spec =
  describe "multipartitions" $ do
    it "calculates correct multipartitions for a sample tree" $ do
      -- t <- removeBrInfo <$> getMultifurcatingTree
      -- print $ multipartitions ex1
      multipartitions ex1 `shouldBe` sol1
      multipartitions ex2 `shouldBe` sol2
    it "is empty for a collection of random bifurcating trees" $
      property (prop_bifurcating_tree :: Tree (PhyloLabel Double) -> Bool)
