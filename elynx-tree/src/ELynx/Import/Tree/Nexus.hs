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

import Control.Applicative
import Data.ByteString (ByteString)
import ELynx.Data.Tree.Phylogeny
import ELynx.Data.Tree.Rooted
import ELynx.Import.Nexus
import ELynx.Import.Tree.Newick
import Data.Attoparsec.ByteString.Char8
import Prelude hiding (takeWhile)

-- | Parse a Nexus files with a TREES block.
nexusTrees :: NewickFormat -> Parser [(ByteString, Tree Phylo ByteString)]
nexusTrees = nexus . trees

trees :: NewickFormat -> Block [(ByteString, Tree Phylo ByteString)]
trees f = Block "TREES" (some $ namedNewick f)

namedNewick :: NewickFormat -> Parser (ByteString, Tree Phylo ByteString)
namedNewick f = do
  _ <- skipWhile isSpace
  _ <- string "TREE"
  _ <- skipWhile isSpace
  n <- takeWhile1 (\x -> isAlpha_ascii x || isDigit x)
  _ <- skipWhile isSpace
  _ <- char '='
  _ <- skipWhile isSpace
  t <- newick f
  return (n, t)
