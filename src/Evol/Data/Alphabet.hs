{-# LANGUAGE AllowAmbiguousTypes #-}

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
  , Character (..)
  ) where

import           Data.Word8    (Word8)


newtype Alphabet = Alphabet { fromAlphabet :: [Word8] }

-- XXX: Maybe remove this type.
data AlphabetName = DNA | DNA_IUPAC | AA
  deriving (Read, Eq, Ord)

instance Show AlphabetName where
  show DNA       = "Nucleotides (DNA)"
  show DNA_IUPAC = "Nucleotides including IUPAC code (DNA_IUPAC)"
  show AA        = "Amino acids (AA)"

-- | The extension @AllowAmbiguousTypes@ is helping a lot here. In conjunction
-- with @ScopedTypeVariables@ and @TypeApplications@. See
-- [StackOverflow](https://stackoverflow.com/questions/41272806/haskell-ambiguous-class-function).
class Show a => Character a where
  word8ToChar  :: Word8 -> a
  -- | Alphabet, uppercase characters only.
  alphabet     :: Alphabet
  -- | Alphabet including lowercase and uppercase character.
  alphabet'    :: Alphabet
  alphabetName :: AlphabetName
