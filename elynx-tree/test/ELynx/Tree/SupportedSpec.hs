{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.SupportedSpec
-- Description :  Unit tests for ELynx.Tree.SupportedSpec
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 21 14:20:09 2020.
module ELynx.Tree.SupportedSpec
  ( spec,
  )
where

import qualified Data.ByteString.Char8 as BS
import ELynx.Tree
import ELynx.Tools
import Test.Hspec

collapseTree :: Tree Phylo BS.ByteString
collapseTree = parseByteStringWith (oneNewick IqTree) "((a,b),(c,d));"

collapseStarTree :: Tree Phylo BS.ByteString
collapseStarTree = parseByteStringWith (oneNewick Standard) "(a[1.0],b[1.0],c[1.0],d[1.0])[1.0];"

spec :: Spec
spec = do
  describe "collapse" $ do
    it "creates a star tree for 1.0" $ do
      let t = phyloToSupportTreeUnsafe collapseTree
          s = phyloToSupportTreeUnsafe collapseStarTree
      collapse 0 t `shouldBe` t
      collapse 0.01 t `shouldBe` t
      collapse 0.99 t `shouldBe` t
      collapse 1.0 t `shouldBe` t
      collapse 1.1 t `shouldBe` s
