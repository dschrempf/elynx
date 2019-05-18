{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Module      :  EvoMod.Data.Nucleotide
Description :  Nucleotides
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

See header of 'EvoMod.Data.Alphabet.Alphabet'.

@
Symbol  Description  Bases represented  Complement
------  -----------  -----------------  ----------
A       Adenine      A                  T
C       Cytosine        C               G
G       Guanine            G            C
T       Thymine               T         A
@

-}

module EvoMod.Data.Alphabet.Nucleotide
  ( Nucleotide (..)
  ) where

import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Character.Character as C
import           EvoMod.Tools.ByteString        (c2w)

-- | Nucleotides.
data Nucleotide = A | C | G | T
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWord :: Nucleotide -> Word8
toWord A = c2w 'A'
toWord C = c2w 'C'
toWord G = c2w 'G'
toWord T = c2w 'T'

fromWord :: Word8 -> Nucleotide
fromWord w | w == c2w 'A' = A
           | w == c2w 'C' = C
           | w == c2w 'G' = G
           | w == c2w 'T' = T
           | otherwise    = error "fromWord: cannot convert to Nucleotide."

derivingUnbox "Nucleotide"
    [t| Nucleotide -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character Nucleotide where
  toWord   = toWord
  fromWord = fromWord
