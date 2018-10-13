{- |
Module      :  Character
Description :  Character interface
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct 12 16:24:02 2018.

A set of characters forms an alphabet.

-}


module Evol.Data.Character
  ( Character (..)
  ) where

import           Data.Word8 (Word8)

class (Enum a, Bounded a) => Character a where
  fromWord :: Word8 -> a
  toWord   :: a -> Word8
