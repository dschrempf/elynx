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
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (float)

import qualified Data.Text as T
import           Data.Tree
import           Data.Void

import           EvoMod.Data.Tree.PhyloTree

-- | A shortcut.
type Parser = Parsec Void T.Text

-- | Parser for many Newick trees.
manyNewick :: Parser [PhyloTextTree]
manyNewick = some (newick <* space) <* eof <?> "manyNewick"

-- | Parser for a Newick tree.
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

forest :: Parser [PhyloTextTree]
forest = char '(' *> tree `sepBy1` char ',' <* char ')' <?> "forest"

leaf :: Parser PhyloTextTree
leaf = do
  n <- node
    <?> "leaf"
  return $ Node n []

node :: Parser PhyloTextNode
node = PhyloNode <$> name <*> branchLength <?> "node"

-- | A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets.
checkNameCharacter :: Char -> Bool
checkNameCharacter c = c `elem` " :;()[]]"

name :: Parser T.Text
name = T.pack <$> many (satisfy checkNameCharacter) <?> "name"

branchLength :: Parser Double
branchLength = char ':' *> float <|> pure 0 <?> "branchLength"
