{-# LANGUAGE ScopedTypeVariables #-}

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

-- module EvoMod.Data.Alphabet.Character
--   ( Character
--   , fromChar
--   , toChar
--   , fromWord8
--   , toWord8
--   , fromString
--   , toString
--   ) where

module EvoMod.Data.Alphabet.Character
  ( Character (..)
  , CharacterX (..)
  , CharacterI (..)
  , fromChar
  , toChar
  , fromString
  , toString
  ) where

import           Data.Vector.Unboxed.Base (Unbox)
import           Data.Word8               (Word8)

import           EvoMod.Tools.ByteString  (c2w, w2c)
import           EvoMod.Tools.Misc        (allValues)

-- | A set of characters forms an 'EvoMod.Data.Alphabet.Alphabet'. At the
-- moment, 'Word8' is used, since none of the alphabets has more than 255
-- characters.
class (Show a, Read a, Eq a, Ord a, Unbox a, Enum a, Bounded a) => Character a where
  -- | Write characters.
  toWord   :: a -> Word8
  -- | Read characters.
  fromWord :: Word8 -> a

  -- | The complete alphabet comprising the code associated with the characters.
  alphabet :: [a]
  alphabet = allValues :: [a]

-- | An extended character type with gaps and unknowns.
class Character a => CharacterX a where
  unknown :: a
  gap     :: a
  toStandard :: a -> [a]

-- | IUPAC characters with a mapping to extended characters.
class CharacterX a => CharacterI a where
  iupac :: [a]

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
