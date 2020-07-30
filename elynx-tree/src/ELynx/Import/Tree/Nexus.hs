{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Import.Tree.Nexus
-- Description :  Import trees from Nexus files
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 17:44:13 2020.
module ELynx.Import.Tree.Nexus
  ( nexusTrees,
  )
where

import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy (ByteString, pack)
import ELynx.Data.Tree.Phylogeny
import ELynx.Data.Tree.Rooted
import ELynx.Import.Nexus hiding (Parser)
import ELynx.Import.Tree.Newick
import Text.Megaparsec
import Text.Megaparsec.Byte

-- | Parse a Nexus files with a TREES block.
nexusTrees :: NewickFormat -> Parser [(ByteString, Tree Phylo ByteString)]
nexusTrees = nexus . trees

trees :: NewickFormat -> Block [(ByteString, Tree Phylo ByteString)]
trees f = Block "TREES" (some $ namedNewick f)

namedNewick :: NewickFormat -> Parser (ByteString, Tree Phylo ByteString)
namedNewick f = do
  _ <- space
  _ <- string "TREE"
  _ <- space
  n <- some alphaNumChar
  _ <- space
  _ <- char $ c2w '='
  _ <- space
  t <- newick f
  return (pack n, t)
