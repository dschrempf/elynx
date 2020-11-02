{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  ELynx.Tree.Import.Newick
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
-- For a description of rooted 'Tree's, please see the 'ELynx.Tree.Rooted'
--
-- Code snippet:
--
-- @
-- import Data.Attoparsec.ByteString
-- import ELynx.Tree
--
-- getOneNewick = either error id . parseOnly (oneNewick Standard)
-- @
module ELynx.Tree.Import.Newick
  ( NewickFormat (..),
    description,
    newick,
    parseNewick,
    oneNewick,
    parseOneNewick,
    someNewick,
    parseSomeNewick,
  )
where

import Control.Applicative
import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import ELynx.Tree.Measurable
import ELynx.Tree.Named
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted hiding (forest, label)
import ELynx.Tree.Supported
import GHC.Generics
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

-- | Newick tree parser. Also succeeds when more trees follow.
newick :: NewickFormat -> Parser (Tree Phylo NodeName)
newick Standard = newickStandard
newick IqTree = newickIqTree
newick RevBayes = newickRevBayes

-- | See 'newick'.
parseNewick :: NewickFormat -> BS.ByteString -> Tree Phylo NodeName
parseNewick f = either error id . parseOnly (newick f)

-- | One Newick tree parser. Fails when end of input is not reached.
oneNewick :: NewickFormat -> Parser (Tree Phylo NodeName)
oneNewick Standard = oneNewickStandard
oneNewick IqTree = oneNewickIqTree
oneNewick RevBayes = oneNewickRevBayes

-- | See 'oneNewick'.
parseOneNewick :: NewickFormat -> BS.ByteString -> Tree Phylo NodeName
parseOneNewick f = either error id . parseOnly (oneNewick f)

-- | One or more Newick trees parser.
someNewick :: NewickFormat -> Parser (Forest Phylo NodeName)
someNewick Standard = someNewickStandard
someNewick IqTree = someNewickIqTree
someNewick RevBayes = someNewickRevBayes

-- | See 'someNewick'.
parseSomeNewick :: NewickFormat -> BS.ByteString -> [Tree Phylo NodeName]
parseSomeNewick f = either error id . parseOnly (someNewick f)

-- Parse a single Newick tree. Also succeeds when more trees follow.
newickStandard :: Parser (Tree Phylo NodeName)
newickStandard = skipWhile isSpace *> tree <* char ';' <* skipWhile isSpace <?> "newickStandard"

-- Parse a single Newick tree. Fails when end of file is not reached.
oneNewickStandard :: Parser (Tree Phylo NodeName)
oneNewickStandard = newickStandard <* endOfInput <?> "oneNewickStandard"

-- Parse one ore more Newick trees until end of file.
someNewickStandard :: Parser (Forest Phylo NodeName)
someNewickStandard = some newickStandard <* endOfInput <?> "someNewickStandard"

tree :: Parser (Tree Phylo NodeName)
tree = branched <|> leaf <?> "tree"

branched :: Parser (Tree Phylo NodeName)
branched = (<?> "branched") $ do
  f <- forest
  n <- name
  p <- phylo
  return $ Node p n f

-- A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser (Forest Phylo NodeName)
forest = char '(' *> (tree `sepBy1` char ',') <* char ')' <?> "forest"

-- A 'leaf' has a 'name' and a 'phylo' branch.
leaf :: Parser (Tree Phylo NodeName)
leaf = (<?> "leaf") $ do
  n <- name
  p <- phylo
  return $ Node p n []

nameChar :: Char -> Bool
nameChar c = c `notElem` " :;()[],"

-- A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets (and commas).
name :: Parser NodeName
name = NodeName . BL.fromStrict <$> takeWhile nameChar <?> "name"

phylo :: Parser Phylo
phylo = Phylo <$> optional branchLength <*> optional branchSupport <?> "phylo"

-- Branch length.
branchLength :: Parser Length
branchLength = do
  _ <- char ':' <?> "branchLengthDelimiter"
  l <- double <?> "branchLength"
  return $ either error id $ toLength l

branchSupport :: Parser Support
branchSupport =
  do
    _ <- char '[' <?> "branchSupportBegin"
    s <- double <?> "branchSupport"
    _ <- char ']' <?> "branchSupportEnd"
    return $ either error id $ toSupport s

--------------------------------------------------------------------------------
-- IQ-TREE.

-- IQ-TREE stores the branch support as node names after the closing bracket of
-- a forest. Parse a single Newick tree. Also succeeds when more trees follow.
newickIqTree :: Parser (Tree Phylo NodeName)
newickIqTree = skipWhile isSpace *> treeIqTree <* char ';' <* skipWhile isSpace <?> "newickIqTree"

-- See 'newickIqTree'. Parse a single Newick tree. Fails when end of file is not
-- reached.
oneNewickIqTree :: Parser (Tree Phylo NodeName)
oneNewickIqTree = newickIqTree <* endOfInput <?> "oneNewickIqTree"

-- See 'newickIqTree'. Parse one ore more Newick trees until end of file.
someNewickIqTree :: Parser (Forest Phylo NodeName)
someNewickIqTree = some newickIqTree <* endOfInput <?> "someNewickIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
treeIqTree :: Parser (Tree Phylo NodeName)
treeIqTree = branchedIqTree <|> leaf <?> "treeIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchedIqTree :: Parser (Tree Phylo NodeName)
branchedIqTree = (<?> "branchedIqTree") $ do
  f <- forestIqTree
  ms <- optional double
  let s = either error id . toSupport <$> ms
  n <- name
  b <- optional branchLength
  return $ Node (Phylo b s) n f

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
forestIqTree :: Parser (Forest Phylo NodeName)
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
-- TODO: Key value pairs are ignored at the moment.
newickRevBayes :: Parser (Tree Phylo NodeName)
newickRevBayes =
  skipWhile isSpace
    *> optional brackets
    *> treeRevBayes
    <* char ';'
    <* skipWhile isSpace <?> "newickRevBayes"

-- See 'newickRevBayes'. Parse a single Newick tree. Fails when end of file is
-- not reached.
oneNewickRevBayes :: Parser (Tree Phylo NodeName)
oneNewickRevBayes = newickRevBayes <* endOfInput <?> "oneNewickRevBayes"

-- See 'newickRevBayes'. Parse one ore more Newick trees until end of file.
someNewickRevBayes :: Parser (Forest Phylo NodeName)
someNewickRevBayes = some newickRevBayes <* endOfInput <?> "someNewickRevBayes"

treeRevBayes :: Parser (Tree Phylo NodeName)
treeRevBayes = branchedRevBayes <|> leafRevBayes <?> "treeRevBayes"

branchedRevBayes :: Parser (Tree Phylo NodeName)
branchedRevBayes = (<?> "branchedRevgBayes") $ do
  f <- forestRevBayes
  n <- nameRevBayes
  b <- optional branchLengthRevBayes
  return $ Node (Phylo b Nothing) n f

forestRevBayes :: Parser (Forest Phylo NodeName)
forestRevBayes = (<?> "forestRevBayes") $ do
  _ <- char '('
  f <- treeRevBayes `sepBy1` char ','
  _ <- char ')'
  return f

nameRevBayes :: Parser NodeName
nameRevBayes = name <* optional brackets <?> "nameRevBayes"

branchLengthRevBayes :: Parser Length
branchLengthRevBayes = branchLength <* optional brackets <?> "branchLengthRevBayes"

leafRevBayes :: Parser (Tree Phylo NodeName)
leafRevBayes = (<?> "leafRevBayes") $ do
  n <- nameRevBayes
  b <- optional branchLengthRevBayes
  return $ Node (Phylo b Nothing) n []

-- Drop anything between brackets.
brackets :: Parser ()
brackets = (<?> "brackets") $ do
  _ <- char '['
  _ <- takeWhile (/= ']')
  _ <- char ']'
  return ()
