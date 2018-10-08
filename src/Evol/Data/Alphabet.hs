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

import           Evol.Data.Defaults (Parser)

data AlphabetName = DNA | AA
  deriving (Read, Eq, Ord)

instance Show AlphabetName where
  show DNA = "Nucleotides (DNA)"
  show AA  = "Amino acids (AA)"

class Show a => Alphabet a where
  parseChar    :: Parser a
  alphabetName :: a -> AlphabetName
