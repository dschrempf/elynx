{-# LANGUAGE DeriveGeneric #-}

-- TODO: Use 'between' for forests.

-- |
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
-- module header.
module ELynx.Import.Tree.Newick
  ( Parser,

    -- * Newick tree format
    NewickFormat (..),
    description,
    newick,
    oneNewick,
    someNewick,
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Void
import Data.Word
import ELynx.Data.Tree hiding (label)
import ELynx.Tools
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
  ( decimal,
    float,
  )

-- | Shortcut.
type Parser = Parsec Void ByteString

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

w :: Char -> Parser Word8
w = char . c2w

-- Parse a single Newick tree. Also succeeds when more trees follow.
newickStandard :: Parser (Tree Phylo ByteString)
newickStandard = space *> tree <* w ';' <* space <?> "newickStandard"

-- Parse a single Newick tree. Fails when end of file is not reached.
oneNewickStandard :: Parser (Tree Phylo ByteString)
oneNewickStandard = newickStandard <* eof <?> "oneNewickStandard"

-- Parse one ore more Newick trees until end of file.
someNewickStandard :: Parser (Forest Phylo ByteString)
someNewickStandard = some newickStandard <* eof <?> "someNewickStandard"

tree :: Parser (Tree Phylo ByteString)
tree = branched <|> leaf <?> "tree"

branched :: Parser (Tree Phylo ByteString)
branched = label "branched" $ do
  f <- forestP
  n <- name
  p <- phylo
  return $ Node p n f

-- A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forestP :: Parser (Forest Phylo ByteString)
forestP = between (w '(') (w ')') (tree `sepBy1` w ',') <?> "forestP"

branchSupport :: Parser (Maybe BranchSupport)
branchSupport = label "branchSupport" $
  optional $
    try $ do
      _ <- w '['
      s <- float <|> decimalAsDouble
      _ <- w ']'
      return s

-- A 'leaf' has a 'name' and a 'phylo' branch.
leaf :: Parser (Tree Phylo ByteString)
leaf = label "leaf" $ do
  n <- name
  p <- phylo
  return $ Node p n []

checkNameCharacter :: Word8 -> Bool
checkNameCharacter c = c `notElem` map c2w " :;()[],"

-- A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets (and commas).
name :: Parser ByteString
name = L.pack <$> many (satisfy checkNameCharacter) <?> "name"

phylo :: Parser Phylo
phylo = Phylo <$> branchLength <*> branchSupport <?> "phylo"

-- Branch length.
branchLength :: Parser (Maybe BranchLength)
branchLength = optional (w ':' *> branchLengthGiven) <?> "branchLength"

branchLengthGiven :: Parser Double
branchLengthGiven = try float <|> decimalAsDouble <?> "branchLengthGiven"

decimalAsDouble :: Parser Double
decimalAsDouble = fromIntegral <$> (decimal :: Parser Int) <?> "decimalAsDouble"

--------------------------------------------------------------------------------
-- IQ-TREE.

-- IQ-TREE stores the branch support as node names after the closing bracket of
-- a forest. Parse a single Newick tree. Also succeeds when more trees follow.
newickIqTree :: Parser (Tree Phylo ByteString)
newickIqTree = space *> treeIqTree <* w ';' <* space <?> "newickIqTree"

-- See 'newickIqTree'. Parse a single Newick tree. Fails when end of file is not
-- reached.
oneNewickIqTree :: Parser (Tree Phylo ByteString)
oneNewickIqTree = newickIqTree <* eof <?> "oneNewickIqTree"

-- See 'newickIqTree'. Parse one ore more Newick trees until end of file.
someNewickIqTree :: Parser (Forest Phylo ByteString)
someNewickIqTree = some newickIqTree <* eof <?> "someNewickIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
treeIqTree :: Parser (Tree Phylo ByteString)
treeIqTree = branchedIqTree <|> leaf <?> "treeIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchedIqTree :: Parser (Tree Phylo ByteString)
branchedIqTree = label "branchedIqTree" $ do
  f <- forestIqTree
  s <- branchSupportIqTree
  n <- name
  b <- branchLength
  return $ Node (Phylo b s) n f

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
forestIqTree :: Parser (Forest Phylo ByteString)
forestIqTree = label "forestIqTree" $ do
  _ <- w '('
  f <- treeIqTree `sepBy1` w ','
  _ <- w ')'
  return f

-- TODO: Same here, why try?
-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchSupportIqTree :: Parser (Maybe BranchSupport)
branchSupportIqTree = label "branchSupportIqTree" $ optional $ try float <|> try decimalAsDouble

--------------------------------------------------------------------------------
-- RevBayes.

-- RevBayes uses square brackets and key-value pairs to define information
-- about nodes and branches. Parse a single Newick tree. Also succeeds when more
-- trees follow.
--
-- XXX: Key value pairs are ignored at the moment.
newickRevBayes :: Parser (Tree Phylo ByteString)
newickRevBayes =
  space *> optional brackets *> treeRevBayes <* w ';' <* space <?> "newickRevBayes"

-- See 'newickRevBayes'. Parse a single Newick tree. Fails when end of file is
-- not reached.
oneNewickRevBayes :: Parser (Tree Phylo ByteString)
oneNewickRevBayes = newickRevBayes <* eof <?> "oneNewickRevBayes"

-- See 'newickRevBayes'. Parse one ore more Newick trees until end of file.
someNewickRevBayes :: Parser (Forest Phylo ByteString)
someNewickRevBayes = some newickRevBayes <* eof <?> "someNewickRevBayes"

treeRevBayes :: Parser (Tree Phylo ByteString)
treeRevBayes = branchedRevBayes <|> leafRevBayes <?> "treeRevBayes"

branchedRevBayes :: Parser (Tree Phylo ByteString)
branchedRevBayes = label "branchedRevgBayes" $ do
  f <- forestRevBayes
  n <- nameRevBayes
  b <- branchLengthRevBayes
  return $ Node (Phylo b Nothing) n f

forestRevBayes :: Parser (Forest Phylo ByteString)
forestRevBayes = label "forestRevBayes" $ do
  _ <- w '('
  f <- treeRevBayes `sepBy1` w ','
  _ <- w ')'
  return f

nameRevBayes :: Parser ByteString
nameRevBayes = name <* optional brackets <?> "nameRevBayes"

branchLengthRevBayes :: Parser (Maybe BranchLength)
branchLengthRevBayes = branchLength <* optional brackets <?> "branchLengthRevBayes"

leafRevBayes :: Parser (Tree Phylo ByteString)
leafRevBayes = label "leafRevBayes" $ do
  n <- nameRevBayes
  b <- branchLengthRevBayes
  return $ Node (Phylo b Nothing) n []

-- Drop anything between brackets.
brackets :: Parser ()
brackets = label "brackets" $ do
  _ <-
    between
      (w '[')
      (w ']')
      (takeWhileP (Just "allCharsButBracketEnd") (/= c2w ']'))
  return ()
