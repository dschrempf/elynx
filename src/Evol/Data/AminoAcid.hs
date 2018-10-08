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
  ) where

import           Control.Applicative
import           Text.Megaparsec.Byte (char')

import           Evol.Data.Alphabet
import           Evol.Defaults        (Parser)
import           Evol.Tools           (c2w)

data AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
  deriving (Show, Read, Eq, Ord)

-- | Parse an amino acid.
-- XXX: Is there a better way to convert Char to Word8?
parseAminoAcid :: Parser AminoAcid
parseAminoAcid = (char' (c2w 'A') >> pure A) <|>
                 (char' (c2w 'C') >> pure C) <|>
                 (char' (c2w 'D') >> pure D) <|>
                 (char' (c2w 'E') >> pure E) <|>
                 (char' (c2w 'F') >> pure F) <|>
                 (char' (c2w 'G') >> pure G) <|>
                 (char' (c2w 'H') >> pure H) <|>
                 (char' (c2w 'I') >> pure I) <|>
                 (char' (c2w 'K') >> pure K) <|>
                 (char' (c2w 'L') >> pure L) <|>
                 (char' (c2w 'M') >> pure M) <|>
                 (char' (c2w 'N') >> pure N) <|>
                 (char' (c2w 'P') >> pure P) <|>
                 (char' (c2w 'Q') >> pure Q) <|>
                 (char' (c2w 'R') >> pure R) <|>
                 (char' (c2w 'S') >> pure S) <|>
                 (char' (c2w 'T') >> pure T) <|>
                 (char' (c2w 'V') >> pure V) <|>
                 (char' (c2w 'W') >> pure W) <|>
                 (char' (c2w 'Y') >> pure Y)

instance Alphabet AminoAcid where
  parseChar = parseAminoAcid
  alphabetName _ = AA
