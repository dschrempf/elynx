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
  , fromString
  , up
  ) where

import           Data.Word8              (Word8, toUpper)
import           EvoMod.Tools.ByteString (c2w)

-- | A set of characters forms an 'EvoMod.Data.Alphabet.Alphabet'. At the
-- moment, 'Word8' is used, since none of the alphabets has more than 255
-- characters.
type Character = Word8

-- | Convert 'Char' into 'Character'.
fromChar :: Char -> Character
fromChar = c2w

-- | Convert 'String' into list of 'Character's.
fromString :: String -> [Character]
fromString = map fromChar

-- | Put character to upper case.
up :: Character -> Character
up = Data.Word8.toUpper
