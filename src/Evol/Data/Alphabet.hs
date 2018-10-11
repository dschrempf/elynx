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
  , ACharacter (..)
  ) where

import qualified Data.Set   as S

-- | List of characters that are accepted. 'Data.Set' is used because it has fast queries.
newtype Alphabet = Alphabet { fromAlphabet :: S.Set Char }

-- | Implemented alphabets.
--
-- XXX: There is a circular dependency here. The different instances of the
-- 'Character' type class depend on the module 'Evol.Data.Alphabet' but this
-- data structure allows is needed to read in the used alphabet.
data AlphabetName = DNA | DNA_IUPAC | AA
  deriving (Read, Eq, Ord)

instance Show AlphabetName where
  show DNA       = "Nucleotides (DNA)"
  show DNA_IUPAC = "Nucleotides including IUPAC code (DNA_IUPAC)"
  show AA        = "Amino acids (AA)"

-- | The extension @AllowAmbiguousTypes@ is helping a lot here. In conjunction
-- with @ScopedTypeVariables@ and @TypeApplications@. See
-- [StackOverflow](https://stackoverflow.com/questions/41272806/haskell-ambiguous-class-function).
--
-- XXX: The 'Character' type class is a little too close CPP classes with
-- inheritance and virtual functions. On the other hand, in order to define a
-- new type of alphabet character, these functions need to be implemented.
class Show a => ACharacter a where
  fromCharToAChar :: Char -> a
  fromACharToChar :: a -> Char
  -- | Alphabet, uppercase characters only.
  alphabet        :: Alphabet
  -- | Alphabet including lowercase and uppercase characters.
  alphabet'       :: Alphabet
  alphabetName    :: AlphabetName
