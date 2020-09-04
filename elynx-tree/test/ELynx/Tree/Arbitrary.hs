{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  ELynx.Tree.Arbitrary
-- Description :  Arbitrary instance, needed for QuickCheck
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 21 17:13:23 2020.
module ELynx.Tree.Arbitrary
  (
  )
where

import Data.Traversable
import ELynx.Tree
import Test.QuickCheck

-- Of course, the boundaries for branch support and length have been chosen
-- pretty arbitrarily :).
--
-- XXX: This instance does not produce values without branch lengths nor branch
-- supports.
instance Arbitrary Phylo where
  arbitrary = Phylo <$> (Just <$> choose (1, 100)) <*> (Just <$> choose (0.001, 10))

instance Arbitrary2 Tree where
  liftArbitrary2 arbB arbN = go
    where
      go = sized $ \n -> do
        -- Sized is the size of the trees.
        br <- arbB
        val <- arbN
        pars <- frequency [(1, pure [1, 1]), (3, arbPartition (n - 1))] -- can go negative!
        frst <- for pars $ \i -> resize i go
        return $ Node br val frst
      arbPartition :: Int -> Gen [Int]
      arbPartition k = case compare k 1 of
        LT -> pure []
        EQ -> pure [1]
        GT -> do
          first <- elements [1 .. k]
          rest <- arbPartition $ k - first
          return $ first : rest

  liftShrink2 _ shrN = go
    where
      go (Node br val frst) =
        frst
          ++ [ Node br e fs
               | (e, fs) <- liftShrink2 shrN (liftShrink go) (val, frst)
             ]

instance (Arbitrary e, Arbitrary a) => Arbitrary (Tree e a) where
  arbitrary = arbitrary2

instance (CoArbitrary e, CoArbitrary a) => CoArbitrary (Tree e a) where
  coarbitrary (Node br val frst) =
    coarbitrary br . coarbitrary val . coarbitrary frst

instance Arbitrary Length where
  arbitrary = Length . getPositive <$> arbitrary
