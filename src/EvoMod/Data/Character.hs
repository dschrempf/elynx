{- |
Module      :  Character
Description :  Character interface
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct 12 16:24:02 2018.

See header of 'EvoMod.Data.Alphabet'.

-}


module EvoMod.Data.Character
  ( Character (..)
  ) where

import           Data.Word8 (Word8)

-- | A set of characters forms an 'EvoMod.Data.Alphabet'. Characters need to
-- support some form of IO. At the moment, I use 'Word8's, since none of my
-- alphabets have more than 255 characters.
--
-- > fromWord . toWord == id
class (Enum a, Bounded a) => Character a where
  fromWord :: Word8 -> a
  toWord   :: a -> Word8
