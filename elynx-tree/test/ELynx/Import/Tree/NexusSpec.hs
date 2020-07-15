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
import ELynx.Data.Tree
import ELynx.Import.Tree.Newick (NewickFormat (..))
import ELynx.Import.Tree.Nexus
import Test.Hspec
import ELynx.Tools

file :: FilePath
file = "data/SimpleTree.nex"

noPL :: Phylo
noPL = Phylo Nothing Nothing

res :: Tree Phylo ByteString
res =
  Node
    noPL
    ""
    [ Node
        noPL
        ""
        [ Node noPL "A" [],
          Node noPL "B" []
        ],
      Node noPL "C" []
    ]

spec :: Spec
spec = describe "trees" $
  it "parses a nexus file with a TREES block" $ do
    ts <- parseFileWith (nexusTrees Standard) file
    head ts `shouldBe` ("tree1", res)
