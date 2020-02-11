{- |
Module      :  ELynx.Import.Tree.Newick
Description :  Import Newick trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 14:56:27 2019.

Code partly taken from Biobase.Newick.Import.

[Specifications](http://evolution.genetics.washington.edu/phylip/newicktree.html)

- In particular, no conversion from _ to (space) is done right now.

-}


module ELynx.Import.Tree.Newick
  ( Parser
  -- * Newick tree format
  , newick
  , oneNewick
  , manyNewick
  , forest
  , leaf
  , node
  , name
  , branchLength
  -- * Newick tree format with branch support as node names (e.g., used by IQ-TREE)
  , newickIqTree
  , oneNewickIqTree
  , manyNewickIqTree
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

import           ELynx.Data.Tree.PhyloTree
import           ELynx.Tools.ByteString         ( c2w )

-- | Shortcut.
type Parser = Parsec Void L.ByteString

-- | Parse a single Newick tree. Also succeeds when more trees follow.
newick :: Parser (Tree (PhyloLabel L.ByteString))
newick = tree <* char (c2w ';') <?> "newick"

-- | Parse a single Newick tree. Fails when end of file is not reached.
oneNewick :: Parser (Tree (PhyloLabel L.ByteString))
oneNewick = newick <* space <* eof <?> "oneNewick"

-- | Parse many Newick trees until end of file.
manyNewick :: Parser [Tree (PhyloLabel L.ByteString)]
manyNewick = some (newick <* space) <* eof <?> "manyNewick"

tree :: Parser (Tree (PhyloLabel L.ByteString))
tree = space *> (branched <|> leaf) <?> "tree"

branched :: Parser (Tree (PhyloLabel L.ByteString))
branched = do
  f <- forest
  n <- node <?> "branched"
  return $ Node n f

-- | A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser [Tree (PhyloLabel L.ByteString)]
forest = do
  _ <- char (c2w '(')
  f <- tree `sepBy1` char (c2w ',')
  _ <- char (c2w ')') <?> "forest"
  return f

branchSupport :: Parser (Maybe Double)
branchSupport = optional $ do
  _ <- try $ char (c2w '[')
  s <- try float <|> try decimalAsDouble
  _ <- try $ char (c2w ']')
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
branchLength =
  (optional $ char (c2w ':') *> branchLengthGiven) <?> "branchLength"

branchLengthGiven :: Parser Double
branchLengthGiven = try float <|> decimalAsDouble

decimalAsDouble :: Parser Double
decimalAsDouble = fromIntegral <$> (decimal :: Parser Int)

--------------------------------------------------------------------------------
-- IQ-TREE STUFF.

-- | IQ-TREE stores the branch support as node names after the closing bracket of a forest.
newickIqTree :: Parser (Tree (PhyloLabel L.ByteString))
newickIqTree = treeIqTree <* char (c2w ';') <?> "newickIqTree"

-- | IQ-TREE stores the branch support as node names after the closing bracket of a forest.
oneNewickIqTree :: Parser (Tree (PhyloLabel L.ByteString))
oneNewickIqTree = newickIqTree <* space <* eof <?> "oneNewickIqTree"

-- | IQ-TREE stores the branch support as node names after the closing bracket of a forest.
manyNewickIqTree :: Parser [Tree (PhyloLabel L.ByteString)]
manyNewickIqTree = some (newickIqTree <* space) <* eof <?> "manyNewickIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
treeIqTree :: Parser (Tree (PhyloLabel L.ByteString))
treeIqTree = space *> (branchedIqTree <|> leaf) <?> "treeIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
forestIqTree :: Parser [Tree (PhyloLabel L.ByteString)]
forestIqTree = do
  _ <- char (c2w '(')
  f <- treeIqTree `sepBy1` char (c2w ',')
  _ <- char (c2w ')') <?> "forestIqTree"
  return f

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchedIqTree :: Parser (Tree (PhyloLabel L.ByteString))
branchedIqTree = do
  f <- forestIqTree
  s <- branchSupportIqTree
  n <- nodeIqTree <?> "branchedIqTree"
  let n' = n { brSup = s }
  return $ Node n' f

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchSupportIqTree :: Parser (Maybe Double)
branchSupportIqTree = optional $ try float <|> try decimalAsDouble

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
nodeIqTree :: Parser (PhyloLabel L.ByteString)
nodeIqTree = do
  n <- name
  b <- branchLength <?> "nodeIqTree"
  return $ PhyloLabel n Nothing b
