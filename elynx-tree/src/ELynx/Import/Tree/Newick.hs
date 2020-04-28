{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  ELynx.Import.Tree.Newick
Description :  Import Newick trees
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 14:56:27 2019.

Some functions are inspired by
[Biobase.Newick.Import](https://hackage.haskell.org/package/BiobaseNewick).

[Specifications](http://evolution.genetics.washington.edu/phylip/newicktree.html)

- In particular, no conversion from _ to (space) is done right now.

TODO: Use 'between' for forests.

-}


module ELynx.Import.Tree.Newick
  ( Parser
  -- * Newick tree format
  , NewickFormat(..)
  , description
  , newick
  , oneNewick
  , manyNewick
  , forest
  , leaf
  , node
  , name
  , branchLength
  )
where

import qualified Data.ByteString.Lazy          as L
import           Data.Tree
import           Data.Void
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer     ( decimal
                                                , float
                                                )

import           ELynx.Data.Tree
import           ELynx.Tools

-- | Shortcut.
type Parser = Parsec Void L.ByteString

-- | Newick tree format.
--
-- >>> unlines $ map (("- " <>) . description) (allValues :: [NewickFormat])
-- - Standard: Branch support values are stored in square brackets after branch lengths.
-- - IqTree:   Branch support values are stored as node names after the closing bracket of forests.
-- - RevBayes  Key-value pairs is provided in square brackets after node names as well as branch lengths. Key value pairs are IGNORED at the moment.
data NewickFormat =
  Standard
  | IqTree
  | RevBayes
  deriving (Eq, Show, Read, Bounded, Enum, Generic)

instance FromJSON NewickFormat

instance ToJSON NewickFormat

-- | Short description of the supported Newick formats.
description :: NewickFormat -> String
description Standard
  = "Standard: Branch support values are stored in square brackets after branch lengths."
description IqTree
  = "IqTree:   Branch support values are stored as node names after the closing bracket of forests."
description RevBayes
  = "RevBayes: Key-value pairs is provided in square brackets after node names as well as branch lengths. XXX: Key value pairs are IGNORED at the moment."

-- | Parse a single Newick tree. Also succeeds when more trees follow.
newick :: NewickFormat -> Parser (Tree (PhyloLabel L.ByteString))
newick Standard = newickStandard
newick IqTree   = newickIqTree
newick RevBayes = newickRevBayes

-- | Parse a single Newick tree. Fails when end of file is not reached.
oneNewick :: NewickFormat -> Parser (Tree (PhyloLabel L.ByteString))
oneNewick Standard = oneNewickStandard
oneNewick IqTree   = oneNewickIqTree
oneNewick RevBayes = oneNewickRevBayes

-- | Parse many Newick trees until end of file.
manyNewick :: NewickFormat -> Parser [Tree (PhyloLabel L.ByteString)]
manyNewick Standard = manyNewickStandard
manyNewick IqTree   = manyNewickIqTree
manyNewick RevBayes = manyNewickRevBayes

w :: Char -> Parser Word8
w = char . c2w

-- | Parse a single Newick tree. Also succeeds when more trees follow.
newickStandard :: Parser (Tree (PhyloLabel L.ByteString))
newickStandard = space *> tree <* w ';' <* space <?> "newick"

-- | Parse a single Newick tree. Fails when end of file is not reached.
oneNewickStandard :: Parser (Tree (PhyloLabel L.ByteString))
oneNewickStandard = newickStandard <* eof <?> "oneNewick"

-- | Parse many Newick trees until end of file.
manyNewickStandard :: Parser [Tree (PhyloLabel L.ByteString)]
manyNewickStandard = some newickStandard <* eof <?> "manyNewick"

tree :: Parser (Tree (PhyloLabel L.ByteString))
tree = branched <|> leaf <?> "tree"

branched :: Parser (Tree (PhyloLabel L.ByteString))
branched = do
  f <- forest
  n <- node <?> "branched"
  return $ Node n f

-- | A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser [Tree (PhyloLabel L.ByteString)]
forest = between (w '(') (w ')') (tree `sepBy1` w ',') <?> "forest"

-- TODO: Why try?
branchSupport :: Parser (Maybe Double)
branchSupport = optional $ do
  _ <- try $ w '['
  s <- try float <|> try decimalAsDouble
  _ <- try $ w ']'
  return s

-- | A 'leaf' is a 'node' without children.
leaf :: Parser (Tree (PhyloLabel L.ByteString))
leaf = do
  n <- node <?> "leaf"
  return $ Node n []

-- | A 'node' has a name and a 'branchLength'.
node :: Parser (PhyloLabel L.ByteString)
node = do
  n <- name
  b <- branchLength
  s <- branchSupport <?> "node"
  return $ PhyloLabel n s b

checkNameCharacter :: Word8 -> Bool
checkNameCharacter c = c `notElem` map c2w " :;()[],"

-- | A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets (and commas).
name :: Parser L.ByteString
name = L.pack <$> many (satisfy checkNameCharacter) <?> "name"

-- | Branch length.
branchLength :: Parser (Maybe Double)
branchLength = optional (w ':' *> branchLengthGiven) <?> "branchLength"

branchLengthGiven :: Parser Double
branchLengthGiven = try float <|> decimalAsDouble

decimalAsDouble :: Parser Double
decimalAsDouble = fromIntegral <$> (decimal :: Parser Int)

--------------------------------------------------------------------------------
-- IQ-TREE.

-- IQ-TREE stores the branch support as node names after the closing bracket of
-- a forest. Parse a single Newick tree. Also succeeds when more trees follow.
newickIqTree :: Parser (Tree (PhyloLabel L.ByteString))
newickIqTree = space *> treeIqTree <* w ';' <* space <?> "newickIqTree"

-- See 'newickIqTree'. Parse a single Newick tree. Fails when end of file is not
-- reached.
oneNewickIqTree :: Parser (Tree (PhyloLabel L.ByteString))
oneNewickIqTree = newickIqTree <* eof <?> "oneNewickIqTree"

-- See 'newickIqTree'. Parse many Newick trees until end of file.
manyNewickIqTree :: Parser [Tree (PhyloLabel L.ByteString)]
manyNewickIqTree = some newickIqTree <* eof <?> "manyNewickIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
treeIqTree :: Parser (Tree (PhyloLabel L.ByteString))
treeIqTree = branchedIqTree <|> leaf <?> "treeIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchedIqTree :: Parser (Tree (PhyloLabel L.ByteString))
branchedIqTree = do
  f <- forestIqTree
  s <- branchSupportIqTree
  n <- nodeIqTree <?> "branchedIqTree"
  let n' = n { brSup = s }
  return $ Node n' f

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
forestIqTree :: Parser [Tree (PhyloLabel L.ByteString)]
forestIqTree = do
  _ <- w '('
  f <- treeIqTree `sepBy1` w ','
  _ <- w ')' <?> "forestIqTree"
  return f

-- TODO: Same here, why try?
-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchSupportIqTree :: Parser (Maybe Double)
branchSupportIqTree = optional $ try float <|> try decimalAsDouble

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
nodeIqTree :: Parser (PhyloLabel L.ByteString)
nodeIqTree = do
  n <- name
  b <- branchLength <?> "nodeIqTree"
  return $ PhyloLabel n Nothing b


--------------------------------------------------------------------------------
-- RevBayes.

-- RevBayes uses square brackets and key-value pairs to define information
-- about nodes and branches. Parse a single Newick tree. Also succeeds when more
-- trees follow.
--
-- XXX: Key value pairs are IGNORED at the moment.
newickRevBayes :: Parser (Tree (PhyloLabel L.ByteString))
newickRevBayes =
  space *> brackets *> treeRevBayes <* w ';' <* space <?> "newickRevBayes"

-- See 'newickRevBayes'. Parse a single Newick tree. Fails when end of file is
-- not reached.
oneNewickRevBayes :: Parser (Tree (PhyloLabel L.ByteString))
oneNewickRevBayes = newickRevBayes <* eof <?> "oneNewickRevBayes"

-- See 'newickRevBayes'. Parse many Newick trees until end of file.
manyNewickRevBayes :: Parser [Tree (PhyloLabel L.ByteString)]
manyNewickRevBayes = some newickRevBayes <* eof <?> "manyNewickRevBayes"

treeRevBayes :: Parser (Tree (PhyloLabel L.ByteString))
treeRevBayes = branchedRevBayes <|> leafRevBayes <?> "treeRevBayes"

branchedRevBayes :: Parser (Tree (PhyloLabel L.ByteString))
branchedRevBayes = do
  f <- forestRevBayes
  n <- nodeRevBayes <?> "branchedRevBayes"
  return $ Node n f

forestRevBayes :: Parser [Tree (PhyloLabel L.ByteString)]
forestRevBayes = do
  _ <- w '('
  f <- treeRevBayes `sepBy1` w ','
  _ <- w ')' <?> "forestRevBayes"
  return f

nodeRevBayes :: Parser (PhyloLabel L.ByteString)
nodeRevBayes = do
  n <- name
  _ <- optional brackets
  b <- branchLength
  _ <- optional brackets <?> "nodeRevBayes"
  return $ PhyloLabel n Nothing b

leafRevBayes :: Parser (Tree (PhyloLabel L.ByteString))
leafRevBayes = do
  n <- nodeRevBayes <?> "leafRevBayes"
  return $ Node n []

-- Drop anything between brackets.
brackets :: Parser ()
brackets = do
  _ <- between (w '[')
               (w ']')
               (takeWhileP (Just "allCharsButBracketEnd") (/= c2w ']'))
  return ()
