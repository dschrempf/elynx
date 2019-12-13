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
  , newick
  , oneNewick
  , manyNewick
  , forest
  , leaf
  , node
  , name
  , branchLength
  ) where

import qualified Data.ByteString.Lazy       as L
import           Data.Tree
import           Data.Void
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer (decimal, float)

import           ELynx.Data.Tree.PhyloTree
import           ELynx.Tools.ByteString     (c2w)

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
  n <- node
    <?> "branched"
  return $ Node n f

-- | A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser [Tree (PhyloLabel L.ByteString)]
forest = do
  _ <- char (c2w '(')
  f <- tree `sepBy1` char (c2w ',')
  _ <- char (c2w ')')
    <?> "forest"
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
  n <- node
    <?> "leaf"
  return $ Node n []

-- | A 'node' has a name and a 'branchLength'.
node :: Parser (PhyloLabel L.ByteString)
node = do
  n <- name
  b <- branchLength
  s <- branchSupport
    <?> "node"
  return $ PhyloLabel n s b

checkNameCharacter :: Word8 -> Bool
checkNameCharacter c = c `notElem` map c2w " :;()[],"

-- | A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets (and commas).
name :: Parser L.ByteString
name = L.pack <$> many (satisfy checkNameCharacter) <?> "name"

-- | Branch lengths default to 0.
branchLength :: Parser Double
branchLength = char (c2w ':') *> branchLengthGiven <|> pure 0 <?> "branchLength"

branchLengthGiven :: Parser Double
branchLengthGiven = try float <|> decimalAsDouble

decimalAsDouble :: Parser Double
decimalAsDouble = fromIntegral <$> (decimal :: Parser Int)
