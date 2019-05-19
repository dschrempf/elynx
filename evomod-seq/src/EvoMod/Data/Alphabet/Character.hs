{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Module      :  EvoMod.Data.Alphabet.Character
Description :  Alphabet characters
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun May 19 21:06:38 2019.

-}

module EvoMod.Data.Alphabet.Character
  (
    Character
  , toWord
  , fromWord
  , toChar
  , fromChar
  , toString
  , fromString
  ) where

import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import           EvoMod.Tools.ByteString        (c2w, w2c)

newtype Character = Character Word8
  deriving (Read, Show, Eq, Ord, Bounded)

toWord :: Character -> Word8
toWord (Character w) = w

fromWord :: Word8 -> Character
fromWord = Character

toChar :: Character -> Char
toChar (Character w) = w2c w

fromChar :: Char -> Character
fromChar = Character . c2w

toString :: [Character] -> String
toString = map toChar

fromString :: String -> [Character]
fromString = map fromChar

derivingUnbox "Character"
    [t| Character -> Word8 |]
    [| \(Character w) -> w |]
    [| Character |]

