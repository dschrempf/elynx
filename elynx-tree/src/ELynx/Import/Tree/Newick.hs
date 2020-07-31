{-# LANGUAGE DeriveGeneric #-}

-- Module      :  ELynx.Import.Tree.Newick
-- Description :  Import Newick trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 14:56:27 2019.
--
-- Some functions are inspired by
-- [Biobase.Newick.Import](https://hackage.haskell.org/package/BiobaseNewick).
--
-- [Specifications](http://evolution.genetics.washington.edu/phylip/newicktree.html)
--
-- In particular, no conversion from _ to (space) is done right now.
--
-- For a description of rooted 'Tree's, please see the 'ELynx.Data.Tree.Rooted'

-- |
-- module header.
module ELynx.Import.Tree.Newick
  ( NewickFormat (..),
    description,
    newick,
    oneNewick,
    someNewick,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import ELynx.Data.Tree.Measurable
import ELynx.Data.Tree.Phylogeny
import ELynx.Data.Tree.Rooted hiding (forest, label)
import ELynx.Data.Tree.Supported
import ELynx.Tools
import Prelude hiding (takeWhile)

-- | Newick tree format.
--
-- >>> unlines $ map (("- " <>) . description) (allValues :: [NewickFormat])
-- - Standard: Branch support values are stored in square brackets after branch lengths.
-- - IqTree:   Branch support values are stored as node names after the closing bracket of forests.
-- - RevBayes: Key-value pairs is provided in square brackets after node names as well as branch lengths. XXX: Key value pairs are ignored at the moment.
data NewickFormat = Standard | IqTree | RevBayes
  deriving (Eq, Show, Read, Bounded, Enum, Generic)

instance FromJSON NewickFormat

instance ToJSON NewickFormat

-- | Short description of the supported Newick formats.
description :: NewickFormat -> String
description Standard =
  "Standard: Branch support values are stored in square brackets after branch lengths."
description IqTree =
  "IqTree:   Branch support values are stored as node names after the closing bracket of forests."
description RevBayes =
  "RevBayes: Key-value pairs is provided in square brackets after node names as well as branch lengths. XXX: Key value pairs are ignored at the moment."

-- | Parse a single Newick tree. Also succeeds when more trees follow.
newick :: NewickFormat -> Parser (Tree Phylo ByteString)
newick Standard = newickStandard
newick IqTree = newickIqTree
newick RevBayes = newickRevBayes

-- | Parse a single Newick tree. Fails when end of file is not reached.
oneNewick :: NewickFormat -> Parser (Tree Phylo ByteString)
oneNewick Standard = oneNewickStandard
oneNewick IqTree = oneNewickIqTree
oneNewick RevBayes = oneNewickRevBayes

-- | Parse one or more Newick trees until end of file.
someNewick :: NewickFormat -> Parser (Forest Phylo ByteString)
someNewick Standard = someNewickStandard
someNewick IqTree = someNewickIqTree
someNewick RevBayes = someNewickRevBayes

-- Parse a single Newick tree. Also succeeds when more trees follow.
newickStandard :: Parser (Tree Phylo ByteString)
newickStandard = many space *> tree <* char ';' <* space <?> "newickStandard"

-- Parse a single Newick tree. Fails when end of file is not reached.
oneNewickStandard :: Parser (Tree Phylo ByteString)
oneNewickStandard = newickStandard <* endOfInput <?> "oneNewickStandard"

-- Parse one ore more Newick trees until end of file.
someNewickStandard :: Parser (Forest Phylo ByteString)
someNewickStandard = some newickStandard <* endOfInput <?> "someNewickStandard"

tree :: Parser (Tree Phylo ByteString)
tree = branched <|> leaf <?> "tree"

branched :: Parser (Tree Phylo ByteString)
branched = (<?> "branched") $ do
  f <- forest
  n <- name
  p <- phylo
  return $ Node p n f

-- A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser (Forest Phylo ByteString)
forest = char '(' *> (tree `sepBy1` char ',') <* char ')' <?> "forest"

-- A 'leaf' has a 'name' and a 'phylo' branch.
leaf :: Parser (Tree Phylo ByteString)
leaf = (<?> "leaf") $ do
  n <- name
  p <- phylo
  return $ Node p n []

nameChar :: Char -> Bool
nameChar c = c `notElem` " :;()[],"

-- A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets (and commas).
name :: Parser ByteString
name = takeWhile nameChar <?> "name"

phylo :: Parser Phylo
phylo = Phylo <$> optional branchLength <*> optional branchSupport <?> "phylo"

-- Branch length.
branchLength :: Parser BranchLength
branchLength = char ':' *> double <?> "branchLength"

branchSupport :: Parser BranchSupport
branchSupport = (<?> "branchSupport") $
  do
    _ <- char '['
    s <- double
    _ <- char ']'
    return s

--------------------------------------------------------------------------------
-- IQ-TREE.

-- IQ-TREE stores the branch support as node names after the closing bracket of
-- a forest. Parse a single Newick tree. Also succeeds when more trees follow.
newickIqTree :: Parser (Tree Phylo ByteString)
newickIqTree = space *> treeIqTree <* char ';' <* space <?> "newickIqTree"

-- See 'newickIqTree'. Parse a single Newick tree. Fails when end of file is not
-- reached.
oneNewickIqTree :: Parser (Tree Phylo ByteString)
oneNewickIqTree = newickIqTree <* endOfInput <?> "oneNewickIqTree"

-- See 'newickIqTree'. Parse one ore more Newick trees until end of file.
someNewickIqTree :: Parser (Forest Phylo ByteString)
someNewickIqTree = some newickIqTree <* endOfInput <?> "someNewickIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
treeIqTree :: Parser (Tree Phylo ByteString)
treeIqTree = branchedIqTree <|> leaf <?> "treeIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchedIqTree :: Parser (Tree Phylo ByteString)
branchedIqTree = (<?> "branchedIqTree") $ do
  f <- forestIqTree
  s <- optional double
  n <- name
  b <- optional branchLength
  return $ Node (Phylo b s) n f

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
forestIqTree :: Parser (Forest Phylo ByteString)
forestIqTree = (<?> "forestIqTree") $ do
  _ <- char '('
  f <- treeIqTree `sepBy1` char ','
  _ <- char ')'
  return f

--------------------------------------------------------------------------------
-- RevBayes.

-- RevBayes uses square brackets and key-value pairs to define information
-- about nodes and branches. Parse a single Newick tree. Also succeeds when more
-- trees follow.
--
-- XXX: Key value pairs are ignored at the moment.
newickRevBayes :: Parser (Tree Phylo ByteString)
newickRevBayes =
  space *> optional brackets *> treeRevBayes <* char ';' <* space <?> "newickRevBayes"

-- See 'newickRevBayes'. Parse a single Newick tree. Fails when end of file is
-- not reached.
oneNewickRevBayes :: Parser (Tree Phylo ByteString)
oneNewickRevBayes = newickRevBayes <* endOfInput <?> "oneNewickRevBayes"

-- See 'newickRevBayes'. Parse one ore more Newick trees until end of file.
someNewickRevBayes :: Parser (Forest Phylo ByteString)
someNewickRevBayes = some newickRevBayes <* endOfInput <?> "someNewickRevBayes"

treeRevBayes :: Parser (Tree Phylo ByteString)
treeRevBayes = branchedRevBayes <|> leafRevBayes <?> "treeRevBayes"

branchedRevBayes :: Parser (Tree Phylo ByteString)
branchedRevBayes = (<?> "branchedRevgBayes") $ do
  f <- forestRevBayes
  n <- nameRevBayes
  b <- optional branchLengthRevBayes
  return $ Node (Phylo b Nothing) n f

forestRevBayes :: Parser (Forest Phylo ByteString)
forestRevBayes = (<?> "forestRevBayes") $ do
  _ <- char '('
  f <- treeRevBayes `sepBy1` char ','
  _ <- char ')'
  return f

nameRevBayes :: Parser ByteString
nameRevBayes = name <* optional brackets <?> "nameRevBayes"

branchLengthRevBayes :: Parser BranchLength
branchLengthRevBayes = branchLength <* optional brackets <?> "branchLengthRevBayes"

leafRevBayes :: Parser (Tree Phylo ByteString)
leafRevBayes = (<?> "leafRevBayes") $ do
  n <- nameRevBayes
  b <- optional branchLengthRevBayes
  return $ Node (Phylo b Nothing) n []

-- Drop anything between brackets.
brackets :: Parser ()
brackets = (<?> "brackets") $ do
  _ <- char '['
  _ <- char ']'
  _ <- takeWhile (/= ']')
  return ()
