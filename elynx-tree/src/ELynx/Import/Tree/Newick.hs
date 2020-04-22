{- |
Module      :  ELynx.Import.Tree.Newick
Description :  Import Newick trees
Copyright   :  (c) Dominik Schrempf 2020
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
  -- * Newick tree format with key-value pais is square backets (e.g., used by RevBayes)
  , newickRevBayes
  , oneNewickRevBayes
  , manyNewickRevBayes
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

w :: Char -> Parser Word8
w = char . c2w

-- | Parse a single Newick tree. Also succeeds when more trees follow.
newick :: Parser (Tree (PhyloLabel L.ByteString))
newick = space *> tree <* w ';' <* space <?> "newick"

-- | Parse a single Newick tree. Fails when end of file is not reached.
oneNewick :: Parser (Tree (PhyloLabel L.ByteString))
oneNewick = newick <* eof <?> "oneNewick"

-- | Parse many Newick trees until end of file.
manyNewick :: Parser [Tree (PhyloLabel L.ByteString)]
manyNewick = some newick <* eof <?> "manyNewick"

tree :: Parser (Tree (PhyloLabel L.ByteString))
tree = branched <|> leaf <?> "tree"

branched :: Parser (Tree (PhyloLabel L.ByteString))
branched = do
  f <- forest
  n <- node <?> "branched"
  return $ Node n f

-- | A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser [Tree (PhyloLabel L.ByteString)]
forest = do
  _ <- w '('
  f <- tree `sepBy1` w ','
  _ <- w ')' <?> "forest"
  return f

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

-- | IQ-TREE stores the branch support as node names after the closing bracket
-- of a forest. Parse a single Newick tree. Also succeeds when more trees
-- follow.
newickIqTree :: Parser (Tree (PhyloLabel L.ByteString))
newickIqTree = space *> treeIqTree <* w ';' <* space <?> "newickIqTree"

-- | IQ-TREE stores the branch support as node names after the closing bracket
-- of a forest. Parse a single Newick tree. Fails when end of file is not
-- reached.
oneNewickIqTree :: Parser (Tree (PhyloLabel L.ByteString))
oneNewickIqTree = newickIqTree <* eof <?> "oneNewickIqTree"

-- | IQ-TREE stores the branch support as node names after the closing bracket
-- of a forest. Parse many Newick trees until end of file.
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

-- | RevBayes uses square brackets and key-value pairs to define information
-- about nodes and branches. Parse a single Newick tree. Also succeeds when more
-- trees follow.
newickRevBayes :: Parser (Tree (PhyloLabel L.ByteString))
newickRevBayes = space *> brackets *> treeRevBayes <* w ';' <* space <?> "newickRevBayes"

-- | RevBayes uses square brackets and key-value pairs to define information
-- about nodes and branches. Parse a single Newick tree. Fails when end of file
-- is not reached.
oneNewickRevBayes :: Parser (Tree (PhyloLabel L.ByteString))
oneNewickRevBayes = newickRevBayes <* eof <?> "oneNewickRevBayes"

-- | RevBayes uses square brackets and key-value pairs to define information
-- about nodes and branches. Parse many Newick trees until end of file.
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
  _ <- between (w '[') (w ']') (takeWhileP (Just "allCharsButBracketEnd") (/= c2w ']'))
  return ()
