{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Module      :  ELynx.Data.Nucleotide
Description :  Nucleotides
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

See header of 'ELynx.Data.Alphabet.Alphabet'.

@
Symbol  Description  Bases represented  Complement
------  -----------  -----------------  ----------
A       Adenine      A                  T
C       Cytosine        C               G
G       Guanine            G            C
T       Thymine               T         A
@

-}

module ELynx.Data.Character.Nucleotide
  ( Nucleotide(..)
  )
where

import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified ELynx.Data.Character.Character
                                               as C
import           ELynx.Tools.ByteString         ( c2w
                                                , w2c
                                                )

-- | Nucleotides.
data Nucleotide = A | C | G | T
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- See https://stackoverflow.com/a/31527024; apparently, pattern matching (and
-- case statements) are fast because they are compiled to lookup tables. Hence,
-- they are faster than guards (because equality has to be checked), and faster
-- than lookups with sets.
toWord :: Nucleotide -> Word8
toWord A = c2w 'A'
toWord C = c2w 'C'
toWord G = c2w 'G'
toWord T = c2w 'T'

fromWord :: Word8 -> Nucleotide
fromWord w = case w2c w of
  'A' -> A
  'C' -> C
  'G' -> G
  'T' -> T
  c   -> error $ "fromWord: Cannot convert " ++ show c ++ " to Nucleotide."

derivingUnbox "Nucleotide"
    [t| Nucleotide -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character Nucleotide where
  toWord   = toWord
  fromWord = fromWord
