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

data AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
  deriving (Show, Read, Eq, Ord)

-- | Parse an amino acid.
-- XXX: Is there a better way to convert Char to Word8?
parseAminoAcid :: Parser AminoAcid
parseAminoAcid = (char' (fromIntegral $ fromEnum 'A') >> pure A) <|>
                 (char' (fromIntegral $ fromEnum 'C') >> pure C) <|>
                 (char' (fromIntegral $ fromEnum 'D') >> pure D) <|>
                 (char' (fromIntegral $ fromEnum 'E') >> pure E) <|>
                 (char' (fromIntegral $ fromEnum 'F') >> pure F) <|>
                 (char' (fromIntegral $ fromEnum 'G') >> pure G) <|>
                 (char' (fromIntegral $ fromEnum 'H') >> pure H) <|>
                 (char' (fromIntegral $ fromEnum 'I') >> pure I) <|>
                 (char' (fromIntegral $ fromEnum 'K') >> pure K) <|>
                 (char' (fromIntegral $ fromEnum 'L') >> pure L) <|>
                 (char' (fromIntegral $ fromEnum 'M') >> pure M) <|>
                 (char' (fromIntegral $ fromEnum 'N') >> pure N) <|>
                 (char' (fromIntegral $ fromEnum 'P') >> pure P) <|>
                 (char' (fromIntegral $ fromEnum 'Q') >> pure Q) <|>
                 (char' (fromIntegral $ fromEnum 'R') >> pure R) <|>
                 (char' (fromIntegral $ fromEnum 'S') >> pure S) <|>
                 (char' (fromIntegral $ fromEnum 'T') >> pure T) <|>
                 (char' (fromIntegral $ fromEnum 'V') >> pure V) <|>
                 (char' (fromIntegral $ fromEnum 'W') >> pure W) <|>
                 (char' (fromIntegral $ fromEnum 'Y') >> pure Y)

instance Alphabet AminoAcid where
  parseChar = parseAminoAcid
  alphabetName _ = AA
