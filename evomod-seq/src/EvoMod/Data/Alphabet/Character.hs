{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

module EvoMod.Data.Alphabet.Character
  ( Character
  , fromChar
  , toChar
  , fromWord8
  , toWord8
  , fromString
  , toString
  ) where

import           Data.Vector.Unboxed.Deriving
import           Data.Word8                   (Word8, toUpper)
import           EvoMod.Tools.ByteString      (c2w, w2c)

-- | A set of characters forms an 'EvoMod.Data.Alphabet.Alphabet'. At the
-- moment, 'Word8' is used, since none of the alphabets has more than 255
-- characters. The value constructor is not exported. This enables (1)
-- enforcement of upper case, and (2) easy change of underlying representation.
newtype Character = Character Word8
  deriving (Show, Read, Eq, Ord)

derivingUnbox "Character"
    [t| Character -> Word8 |]
    [| \(Character w) -> w |]
    [| Character |]

-- | Convert 'Char' into 'Character', upper case is enforced.
fromChar :: Char -> Character
fromChar = Character . toUpper . c2w

-- | Convert 'Character' into 'Char'.
toChar :: Character -> Char
toChar (Character w) = w2c w

-- | Convert 'Word8' into 'Character', upper case is enforced.
fromWord8 :: Word8 -> Character
fromWord8 = Character . toUpper

-- | Convert 'Character into ''Word8'.
toWord8 :: Character -> Word8
toWord8 (Character w) = w

-- | Convert 'String' into list of 'Character's.
fromString :: String -> [Character]
fromString = map fromChar

-- | Convert list of 'Character's into String.
toString :: [Character] -> String
toString = map toChar
