{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module      :  Character
Description :  Character interface
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct 12 16:24:02 2018.

See header of 'EvoMod.Data.Alphabet.Alphabet'.

-}

module EvoMod.Data.Character.Character
  (
    Character (..)
  , fromChar
  , toChar
  , fromString
  , toString
  , CharacterX (..)
  , isGap
  , CharacterI (..)
  , isIUPAC
  , isStandard
  ) where

import qualified Data.Set                 as S
import           Data.Vector.Unboxed.Base (Unbox)
import           Data.Word8               (Word8)

import           EvoMod.Tools.ByteString  (c2w, w2c)


-- XXX: Remove name clash with EvoMod.Data.Alphabet.Alphabet.Character?
-- | A set of characters forms an 'EvoMod.Data.Alphabet.Alphabet'. At the
-- moment, 'Word8' is used, since none of the alphabets has more than 255
-- characters.
class (Show a, Read a, Eq a, Ord a, Enum a, Bounded a, Unbox a) => Character a where
  -- | Write characters.
  toWord   :: a -> Word8
  -- | Read characters.
  fromWord :: Word8 -> a

-- | Conversion to 'Char'.
toChar :: Character a => a -> Char
toChar = w2c . toWord

-- | Conversion from 'Char'.
fromChar :: Character a => Char -> a
fromChar = fromWord . c2w

-- | Conversion to 'String'.
toString :: Character a => [a] -> String
toString = map toChar

  -- | Conversion from 'String'.
fromString :: Character a => String -> [a]
fromString = map fromChar

-- | An extended character type with gaps and unknowns.
class Character a => CharacterX a where
  gap     :: a

-- | Is the character a gap or unknown?
isGap :: CharacterX a => a -> Bool
isGap c = c == gap

-- | IUPAC characters with a mapping to extended characters.
class CharacterX a => CharacterI a where
  iupac :: [a]
  toStandard :: a -> [a]

iupacLookup :: CharacterI a => S.Set a
iupacLookup = S.fromList iupac

-- | Is the given character a IUPAC character?
isIUPAC :: CharacterI a => a -> Bool
isIUPAC c = c `S.member` iupacLookup

-- | Is the given character a standard character?
isStandard :: CharacterI a => a -> Bool
isStandard c = not $ isIUPAC c
