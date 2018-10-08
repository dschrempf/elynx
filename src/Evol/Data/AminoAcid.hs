{- |
Module      :  Evol.Data.AminoAcid
Description :  Amino acid related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

-}


module Evol.Data.AminoAcid
  ( AminoAcid (..)
  , parseAminoAcid
  , parseAminoAcidSequence
  ) where

import           Control.Applicative
import           Text.Megaparsec.Char (char')

import           Evol.Data.Alphabet
import           Evol.Data.Defaults        (Parser)
import           Evol.Data.Sequence

data AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
  deriving (Show, Read, Eq, Ord)

parseAminoAcid :: Parser AminoAcid
parseAminoAcid = (char' 'A' >> pure A) <|>
                 (char' 'C' >> pure C) <|>
                 (char' 'D' >> pure D) <|>
                 (char' 'E' >> pure E) <|>
                 (char' 'F' >> pure F) <|>
                 (char' 'G' >> pure G) <|>
                 (char' 'H' >> pure H) <|>
                 (char' 'I' >> pure I) <|>
                 (char' 'K' >> pure K) <|>
                 (char' 'L' >> pure L) <|>
                 (char' 'M' >> pure M) <|>
                 (char' 'N' >> pure N) <|>
                 (char' 'P' >> pure P) <|>
                 (char' 'Q' >> pure Q) <|>
                 (char' 'R' >> pure R) <|>
                 (char' 'S' >> pure S) <|>
                 (char' 'T' >> pure T) <|>
                 (char' 'V' >> pure V) <|>
                 (char' 'W' >> pure W) <|>
                 (char' 'Y' >> pure Y)

instance Alphabet AminoAcid where
  parseChar = parseAminoAcid
  alphabetName _ = AA

parseAminoAcidSequence :: Parser (Sequence AminoAcid)
parseAminoAcidSequence = parseSequence
