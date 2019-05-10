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

-- TODO: I have to change the basic type classes. It feels like I have to
-- implement everything twice. See *W functions in
-- "EvoMod.Data.Alphabet.Alphabet".

-- Somehow, IUPAC characters should be separated from 'Character'. But then,
-- function definitions require a CharacterIUPAC constraint, and cannot be used
-- for normal Characters.

module EvoMod.Data.Alphabet.Character
  ( Character
  , fromChar
  , fromString
  ) where

import           Data.Word8              (Word8)
import           EvoMod.Tools.ByteString (c2w)

-- | A set of characters forms an 'Alphabet'. At the moment, 'Word8' is used,
-- since none of the alphabets has more than 255 characters.
type Character = Word8

-- | Convert 'Char' into 'Character'.
fromChar :: Char -> Character
fromChar = c2w

-- | Convert 'String' into list of 'Character's.
fromString :: String -> [Character]
fromString = fromString

-- -- | A set of characters forms an 'EvoMod.Data.Alphabet'. Characters need to
-- -- support some form of IO. At the moment, I use 'Word8's, since none of my
-- -- alphabets have more than 255 characters.
-- --
-- -- > fromWord . toWord == id
-- class (Enum a, Bounded a) => Character a where
--   fromWord :: Word8 -> a
--   toWord   :: a -> Word8
--   -- This should probably go into its own type class, but then everything is
--   -- more complicated.
--   isStandard :: a -> Bool
--   isGapOrUnknown :: a -> Bool

--   isIUPACChar :: a -> Bool
--   isIUPACChar = not . isStandard

-- -- class (Enum a, Bounded a, Character a) => CharacterIUPAC a where
-- --   isStandard :: a -> Bool
-- --   isGapOrUnknown :: a -> Bool

-- --   isIUPACChar :: a -> Bool
-- --   isIUPACChar = not . isStandard
