{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  ELynx.Tree.PhylogenySpec
-- Description :  Unit tests for ELynx.Tree.Phylogeny
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed Jul 15 11:05:32 2020.
module ELynx.Tree.PhylogenySpec
  ( spec,
  )
where

import Data.Either
import qualified Data.Set as S
import ELynx.Tree
import ELynx.Tree.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (labels)

simpleTree1 :: Tree () String
simpleTree1 = Node () "i" [Node () "j" [Node () "x" [], Node () "y" []], Node () "z" []]

simpleTree2 :: Tree () String
simpleTree2 = Node () "i" [Node () "j" [Node () "y" [], Node () "x" []], Node () "z" []]

simpleTree3 :: Tree () String
simpleTree3 = Node () "i" [Node () "j" [Node () "x" [], Node () "z" []], Node () "y" []]

prop_commutative :: (Eq a, Ord a) => Tree () a -> Tree () a -> Bool
prop_commutative t1 t2 = case (s1, s2) of
  (Left _, Left _) -> True
  (Right x1, Right x2) -> x1 == x2
  _ -> False
  where
    s1 = t1 `equal` t2
    s2 = t2 `equal` t1

simpleSol :: Forest () String
simpleSol =
  [ Node () "i" [Node () "j" [Node () "x" [], Node () "y" []], Node () "z" []],
    Node () "i" [Node () "j" [Node () "z" [], Node () "y" []], Node () "x" []],
    Node () "i" [Node () "j" [Node () "z" [], Node () "x" []], Node () "y" []]
  ]

instance Splittable () where
  split = id

-- Skip leaves and trees with multifurcating root nodes.
prop_roots :: Tree () a -> Bool
prop_roots t@(Node _ _ [_, _])
  | length (leaves t) == 2 = (length <$> roots t) == Right 1
  | otherwise = (length <$> roots t) == Right (length (labels t) - 2)
prop_roots _ = True

-- -- Skip leaves and trees with multifurcating root nodes.
-- prop_connect :: a -> Tree () a -> Tree () a -> Bool
-- prop_connect n l@(Node _ _ [_, _]) r@(Node _ _ [_, _])
--   | length (leaves l) < 3 = (length <$> connect n l r) == Right (length (flatten r) - 2)
--   | length (leaves r) < 3 = (length <$> connect n l r) == Right (length (flatten l) - 2)
--   | otherwise =
--     (length <$> connect n l r)
--       == (Right $ (length (flatten l) - 2) * (length (flatten r) - 2))
-- prop_connect _ _ _ = True

-- -- | Determine compatibility between a bipartition and a set.
-- --
-- -- If both subsets of the bipartition share elements with the given set, the
-- -- bipartition is incompatible with this subset. If all elements of the subset
-- -- are either not in the bipartition or mapping to one of the two subsets of the
-- -- bipartition, the bipartition and the subset are compatible.
-- --
-- -- See also 'ELynx.Tree.Partition.compatible'.
-- bipartitionCompatible :: (Show a, Ord a) => Either String (Bipartition a) -> Set a -> Bool
-- -- compatible (Bipartition (l, r)) ss = sintersection l ss `sdisjoint` sintersection r ss
-- bipartitionCompatible (Left _) _ = False
-- bipartitionCompatible (Right p) s = S.null lOverlap || S.null rOverlap
--   where
--     (l, r) = fromBipartition p
--     lOverlap = S.intersection l s
--     rOverlap = S.intersection r s

-- compatibleAll :: (Show a, Ord a) => Tree e a -> [Set a] -> Bool
-- compatibleAll (Node _ _ [l, r]) cs =
--   all (bipartitionCompatible (bipartition l)) cs && all (bipartitionCompatible (bipartition r)) cs
-- compatibleAll _ _ = error "Tree is not bifurcating."

-- compatibleWith ::
--   (Show b, Ord b) => (a -> b) -> [Set a] -> Tree e a -> Bool
-- compatibleWith f cs t = compatibleAll (fmap f t) (map (S.map f) cs)

-- -- Get groups induced by multifurcations. Collect the leaves of all trees
-- -- induced by multifurcations.
-- multifurcatingGroups :: Tree e a -> [[a]]
-- multifurcatingGroups (Node _ _ []) = []
-- multifurcatingGroups (Node _ _ [x]) = multifurcatingGroups x
-- multifurcatingGroups (Node _ _ [x, y]) = multifurcatingGroups x ++ multifurcatingGroups y
-- multifurcatingGroups t = leaves t : concatMap multifurcatingGroups (forest t)

-- -- TODO.
-- prop_bifurcating_tree
--   :: (Ord a, HasLength a, HasName a, HasSupport a) => Tree a -> Bool
-- prop_bifurcating_tree t = partitions (resolve t) == empty

prop_roots_total_length :: Tree Length a -> Bool
prop_roots_total_length t@(Node _ _ [_, _]) =
  all (\x -> abs (totalBranchLength x - l) < 1e-8) $
    either error id $
      roots t
  where
    l = totalBranchLength t
prop_roots_total_length _ = True

spec :: Spec
spec = do
  describe "equal" $ do
    it "correctly handles some test cases" $ do
      simpleTree1 `equal` simpleTree2 `shouldBe` Right True
      simpleTree1 `equal` simpleTree3 `shouldBe` Right False
      simpleTree2 `equal` simpleTree3 `shouldBe` Right False
    it "is commutative" $
      property (prop_commutative :: Tree () Int -> Tree () Int -> Bool)
  describe "roots" $ do
    it "correctly handles leaves and cherries" $ do
      let tleaf = Node () 0 [] :: Tree () Int
          tcherry = Node () 0 [Node () 1 [], Node () 2 []] :: Tree () Int
      roots tleaf `shouldSatisfy` isLeft
      roots tcherry `shouldBe` Right [tcherry]
    it "correctly handles simple trees" $
      either error id (roots simpleTree1) `shouldBe` simpleSol
    modifyMaxSize (* 100) $
      it "returns the correct number of rooted trees for arbitrary trees" $
        property (prop_roots :: (Tree () Int -> Bool))
  describe "outgroup" $
    modifyMaxSize (* 100) $
      it "correctly handles simple trees" $
        do
          let p = fst $ fromBipartition $ either error id $ bipartition simpleTree1
          outgroup p simpleTree1 `shouldBe` Right simpleTree1
          let l = S.singleton "x"
          either error id (outgroup l simpleTree1) `equal` (simpleSol !! 1)
            `shouldBe` Right True
  describe "rootsWithBranch" $
    modifyMaxSize (* 100) $
      it "does not change the tree height" $
        property (prop_roots_total_length :: Tree Length Int -> Bool)

-- -- TODO: Move this test to the executable.
-- describe "connect" $
--   modifyMaxSize (* 100) $ do
--     it "returns the correct number of rooted trees for arbitrary trees" $
--       property (prop_connect :: Int -> Tree () Int -> Tree () Int -> Bool)
--     it "correctly connects sample trees without and with constraints" $ do
--       a <- parseFileWith (oneNewick Standard) "data/ConnectA.tree"
--       b <- parseFileWith (oneNewick Standard) "data/ConnectB.tree"
--       c <- parseFileWith (someNewick Standard) "data/ConnectConstraints.tree"
--       let ts =
--             either error id $
--               connect "ROOT" (first (const ()) a) (first (const ()) b)
--           cs =
--             map S.fromList $
--               concatMap (multifurcatingGroups . first (const ())) c ::
--               [Set ByteString]
--           ts' = filter (compatibleWith getName cs) ts
--       length ts `shouldBe` 63
--       length ts' `shouldBe` 15
