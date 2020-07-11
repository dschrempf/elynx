{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Import.Tree.NexusSpec
-- Description :  Test import of trees in Nexus files
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 18:08:14 2020.
module ELynx.Import.Tree.NexusSpec
  ( spec,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Tree
import ELynx.Data.Tree
import ELynx.Import.Tree.Newick (NewickFormat (..))
import ELynx.Import.Tree.Nexus
import Test.Hspec
import ELynx.Tools

file :: FilePath
file = "data/SimpleTree.nex"

res :: Tree (PhyloLabelSoft ByteString)
res =
  Node
    (PhyloLabelSoft "" Nothing Nothing)
    [ Node
        (PhyloLabelSoft "" Nothing Nothing)
        [ Node (PhyloLabelSoft "A" Nothing Nothing) [],
          Node (PhyloLabelSoft "B" Nothing Nothing) []
        ],
      Node (PhyloLabelSoft "C" Nothing Nothing) []
    ]

spec :: Spec
spec = describe "trees" $
  it "parses a nexus file with a TREES block" $ do
    ts <- parseFileWith (nexusTrees Standard) file
    head ts `shouldBe` ("tree1", res)
