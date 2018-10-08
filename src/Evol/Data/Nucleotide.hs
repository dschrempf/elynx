{- |
Module      :  Evol.Data.Nucleotide
Description :  Nucleotide related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

-}


module Evol.Data.Nucleotide
  ( Nucleotide (..)
  , parseNucleotide
  , parseNucleotideWord8
  ) where

import           Data.Word8           (Word8, toUpper)
import           Text.Megaparsec      (oneOf)

import           Evol.Data.Alphabet
import           Evol.Defaults        (Parser)
import           Evol.Tools           (c2w)

data Nucleotide = A | C | G | T deriving (Show, Read, Eq, Ord)

word8ToNucleotide :: Word8 -> Nucleotide
word8ToNucleotide w | w' == c2w 'A' = A
                    | w' == c2w 'C' = C
                    | w' == c2w 'G' = G
                    | w' == c2w 'T' = T
                    | otherwise = error $ "Cannot read nucleotide " ++ show w
  where w' = toUpper w


-- | Parse a nucleotide.
-- parseNucleotide :: Parser Nucleotide
-- parseNucleotide = (char' (c2w 'A') >> pure A) <|>
--                   (char' (c2w 'C') >> pure C) <|>
--                   (char' (c2w 'G') >> pure G) <|>
--                   (char' (c2w 'T') >> pure T)
parseNucleotide :: Parser Nucleotide
parseNucleotide = word8ToNucleotide <$> parseNucleotideWord8

parseNucleotideWord8 :: Parser Word8
parseNucleotideWord8 = oneOf $ map c2w ['A', 'C', 'G', 'T', 'a', 'c', 'g', 't']

instance Alphabet Nucleotide where
  parseChar = parseNucleotide
  alphabetName _ = DNA

-- data NucleotideIUPAC = ...
