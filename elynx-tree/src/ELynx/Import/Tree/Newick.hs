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

-- | Parse many Newick trees.
manyNewick :: Parser [Tree PhyloByteStringLabel]
manyNewick = some (newick <* space) <* eof <?> "manyNewick"

-- | Parse a Newick tree.
newick :: Parser (Tree PhyloByteStringLabel)
newick = tree <* char (c2w ';') <?> "newick"

tree :: Parser (Tree PhyloByteStringLabel)
tree = space *> (branched <|> leaf) <?> "tree"

branched :: Parser (Tree PhyloByteStringLabel)
branched = do
  f <- forest
  s <- branchSupport
  n <- node
    <?> "branched"
  let n' = n {pBrSup = s}
  return $ Node n' f

-- | A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser [Tree PhyloByteStringLabel]
forest = do
  _ <- char (c2w '(')
  f <- tree `sepBy1` char (c2w ',')
  _ <- char (c2w ')')
    <?> "forest"
  return f

branchSupport :: Parser (Maybe Double)
branchSupport = optional $ try float <|> try decimalAsDouble

-- | A 'leaf' is a 'node' without children.
leaf :: Parser (Tree PhyloByteStringLabel)
leaf = do
  n <- node
    <?> "leaf"
  return $ Node n []

-- | A 'node' has a name and a 'branchLength'.
node :: Parser PhyloByteStringLabel
node = do
  n <- name
  b <- branchLength
    <?> "node"
  return $ PhyloLabel n Nothing b

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
