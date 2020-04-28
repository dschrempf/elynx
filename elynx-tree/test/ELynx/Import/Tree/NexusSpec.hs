{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Import.Tree.NexusSpec
Description :  Test import of trees in Nexus files
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Apr 28 18:08:14 2020.

-}

module ELynx.Import.Tree.NexusSpec
  ( spec
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Tree
import Test.Hspec

import ELynx.Data.Tree.PhyloTree
import ELynx.Import.Tree.Newick (NewickFormat(..))
import ELynx.Import.Tree.Nexus
import ELynx.Tools

file :: FilePath
file = "data/SimpleTree.nex"

res :: Tree (PhyloLabel ByteString)
res = Node (PhyloLabel "" Nothing Nothing)
  [ Node (PhyloLabel "" Nothing Nothing)
    [ Node (PhyloLabel "A" Nothing Nothing) []
    , Node (PhyloLabel "B" Nothing Nothing) []]
  , Node (PhyloLabel "C" Nothing Nothing) [] ]

spec :: Spec
spec = describe "trees" $ it "parses a nexus file with a TREES block" $ do
  ts <- parseFileWith (nexus $ trees Standard) file
  head ts `shouldBe` ("tree1", res)

