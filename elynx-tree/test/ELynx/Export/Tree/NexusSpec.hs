{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Export.Tree.NexusSpec
Description :  Test export of trees in Nexus files
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Apr 28 18:08:14 2020.

-}

module ELynx.Export.Tree.NexusSpec
  ( spec
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Tree
import Test.Hspec

import ELynx.Data.Tree
import ELynx.Import.Tree.Newick (NewickFormat(..))
import ELynx.Import.Tree.Nexus
import ELynx.Export.Tree.Nexus
import ELynx.Tools

tree :: Tree (PhyloLabelSoft ByteString)
tree = Node (PhyloLabelSoft "" Nothing Nothing)
  [ Node (PhyloLabelSoft "" Nothing Nothing)
    [ Node (PhyloLabelSoft "A" Nothing Nothing) []
    , Node (PhyloLabelSoft "B" Nothing Nothing) []]
  , Node (PhyloLabelSoft "C" Nothing Nothing) [] ]

spec :: Spec
spec = describe "toNexusTrees" $ it "exports a nexus file with a TREES block" $ do
  let ts = parseByteStringWith "NexusTrees" (nexusTrees Standard) (toNexusTrees [("tree1", tree)])
  head ts `shouldBe` ("tree1", tree)
