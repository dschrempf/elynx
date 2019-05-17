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
  , CharacterI (..)
  , fromChar
  , toChar
  , fromString
  , toString
  ) where

import           Data.Vector.Unboxed.Base (Unbox)
import           Data.Word8               (Word8, toUpper)

import           EvoMod.Tools.ByteString  (c2w, w2c)
import           EvoMod.Tools.Misc        (allValues)

class (Show a, Read a, Eq a, Ord a, Unbox a, Enum a, Bounded a) => Character a where
  toWord   :: a -> Word8
  fromWord :: Word8 -> a

  alphabet :: [a]
  alphabet = allValues :: [a]

class Character a => CharacterI a where
  standard :: [a]
  gap      :: [a]
  unknown  :: [a]

toChar :: Character a => a -> Char
toChar = w2c . toWord

fromChar :: Character a => Char -> a
fromChar = fromWord . toUpper . c2w

toString :: Character a => [a] -> String
toString = map toChar

fromString :: Character a => String -> [a]
fromString = map fromChar
