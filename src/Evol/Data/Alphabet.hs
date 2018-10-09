{- |
Module      :  Evol.Data.Alphabet
Description :  Alphabets store hereditary information.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:57:08 2018.

-}


module Evol.Data.Alphabet
  ( Alphabet (..)
  , AlphabetName (..)
  ) where

import           Data.Word8    (Word8)

data AlphabetName = DNA | DNA_IUPAC | AA
  deriving (Read, Eq, Ord)

instance Show AlphabetName where
  show DNA       = "Nucleotides (DNA)"
  show DNA_IUPAC = "Nucleotides including IUPAC code (DNA_IUPAC)"
  show AA        = "Amino acids (AA)"

-- TODO: REMOVE TYPE CLASS. Use algebraic data type. data Alphabet = DNA | DNA_IUPAC | ...
class Show a => Alphabet a where
  alphabet      :: a -> [Word8]
  word8ToChar   :: Word8 -> a
  alphabetName  :: a -> AlphabetName
