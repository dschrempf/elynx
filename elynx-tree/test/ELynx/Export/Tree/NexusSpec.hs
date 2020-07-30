{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Export.Tree.NexusSpec
-- Description :  Test export of trees in Nexus files
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 18:08:14 2020.
module ELynx.Export.Tree.NexusSpec
  ( spec,
  )
where

import Data.ByteString.Lazy (ByteString)
import ELynx.Data.Tree
import ELynx.Export.Tree.Nexus
import ELynx.Import.Tree.Newick (NewickFormat (..))
import ELynx.Import.Tree.Nexus
import ELynx.Tools
import Test.Hspec

tree :: Tree Phylo ByteString
tree =
  Node
    (Phylo Nothing Nothing)
    ""
    [ Node
        (Phylo Nothing Nothing)
        ""
        [ Node (Phylo Nothing Nothing) "A" [],
          Node (Phylo Nothing Nothing) "B" []
        ],
      Node (Phylo Nothing Nothing) "C" []
    ]

spec :: Spec
spec = describe "toNexusTrees" $
  it "exports a nexus file with a TREES block" $
    do
      let ts = parseByteStringWith "NexusTrees" (nexusTrees Standard) (toNexusTrees [("tree1", tree)])
      head ts `shouldBe` ("tree1", tree)
