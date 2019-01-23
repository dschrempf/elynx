{- |
Module      :  EvoMod.Import.Tree.Newick
Description :  Import Newick trees.
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


module EvoMod.Import.Tree.Newick
  ( Parser
  , newick
  , manyNewick
  , forest
  , leaf
  , node
  , name
  , branchLength
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal, float)

import qualified Data.Text                  as T
import           Data.Tree
import           Data.Void

import           EvoMod.Data.Tree.PhyloTree

-- | A shortcut.
type Parser = Parsec Void T.Text

-- | Parse many Newick trees.
manyNewick :: Parser [PhyloTextTree]
manyNewick = some (newick <* space) <* eof <?> "manyNewick"

-- | Parse a Newick tree.
newick :: Parser PhyloTextTree
newick = tree <* char ';' <?> "newick"

tree :: Parser PhyloTextTree
tree = space *> (branched <|> leaf) <?> "tree"

branched :: Parser PhyloTextTree
branched = do
  f <- forest
  n <- node
    <?> "branched"
  return $ Node n f

-- | A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser [PhyloTextTree]
forest = char '(' *> tree `sepBy1` char ',' <* char ')' <?> "forest"

-- | A 'leaf' is a 'node' without children.
leaf :: Parser PhyloTextTree
leaf = do
  n <- node
    <?> "leaf"
  return $ Node n []

-- | A 'node' has a name and a 'branchLength'.
node :: Parser PhyloTextNode
node = do
  n <- name
  b <- branchLength
    <?> "node"
  return $ PhyloNode n b

checkNameCharacter :: Char -> Bool
checkNameCharacter c = c `notElem` " :;()[],"

-- | A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets (and commas).
name :: Parser T.Text
name = T.pack <$> many (satisfy checkNameCharacter) <?> "name"

-- | Branch lengths default to 0.
branchLength :: Parser Double
branchLength = char ':' *> branchLengthGiven <|> pure 0 <?> "branchLength"

branchLengthGiven :: Parser Double
branchLengthGiven = try float <|> (fromIntegral <$> (decimal :: Parser Int))
