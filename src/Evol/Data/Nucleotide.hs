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
  ) where

import           Control.Applicative
import           Text.Megaparsec.Byte (char')

import           Evol.Data.Alphabet
import           Evol.Defaults        (Parser)

data Nucleotide = A | C | G | T deriving (Show, Read, Eq, Ord)


-- | Parse a nucleotide.
-- XXX: Is there a better way to convert Char to Word8?
parseNucleotide :: Parser Nucleotide
parseNucleotide = (char' (fromIntegral $ fromEnum 'A') >> pure A) <|>
                  (char' (fromIntegral $ fromEnum 'C') >> pure C) <|>
                  (char' (fromIntegral $ fromEnum 'G') >> pure G) <|>
                  (char' (fromIntegral $ fromEnum 'T') >> pure T)

instance Alphabet Nucleotide where
  parseChar = parseNucleotide
  alphabetName _ = DNA

-- data NucleotideIUPAC = ...
