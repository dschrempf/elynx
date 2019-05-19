{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Module      :  EvoMod.Data.AminoAcid
Description :  Amino acid related types and functions
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

See header of 'EvoMod.Data.Alphabet.Alphabet'.

Amino acids in alphabetical order.

@
Amino Acid Code:  Three letter Code:  Amino Acid:
----------------  ------------------  -----------
A                 Ala                 Alanine
C                 Cys                 Cysteine
D                 Asp                 Aspartic Acid
E                 Glu                 Glutamic Acid
F                 Phe                 Phenylalanine
G                 Gly                 Glycine
H                 His                 Histidine
I                 Ile                 Isoleucine
K                 Lys                 Lysine
L                 Leu                 Leucine
M                 Met                 Methionine
N                 Asn                 Asparagine
P                 Pro                 Proline
Q                 Gln                 Glutamine
R                 Arg                 Arginine
S                 Ser                 Serine
T                 Thr                 Threonine
V                 Val                 Valine
W                 Trp                 Tryptophan
Y                 Tyr                 Tyrosine
@

-}

module EvoMod.Data.Alphabet.AminoAcid
  ( AminoAcid (..)
  ) where

import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Character.Character as C
import           EvoMod.Tools.ByteString        (c2w, w2c)

-- | Amino acids.
data AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWord :: AminoAcid -> Word8
toWord A = c2w 'A'
toWord C = c2w 'C'
toWord D = c2w 'D'
toWord E = c2w 'E'
toWord F = c2w 'F'
toWord G = c2w 'G'
toWord H = c2w 'H'
toWord I = c2w 'I'
toWord K = c2w 'K'
toWord L = c2w 'L'
toWord M = c2w 'M'
toWord N = c2w 'N'
toWord P = c2w 'P'
toWord Q = c2w 'Q'
toWord R = c2w 'R'
toWord S = c2w 'S'
toWord T = c2w 'T'
toWord V = c2w 'V'
toWord W = c2w 'W'
toWord Y = c2w 'Y'

fromWord :: Word8 -> AminoAcid
fromWord w = case w2c w of
               'A' -> A
               'C' -> C
               'D' -> D
               'E' -> E
               'F' -> F
               'G' -> G
               'H' -> H
               'I' -> I
               'K' -> K
               'L' -> L
               'M' -> M
               'N' -> N
               'P' -> P
               'Q' -> Q
               'R' -> R
               'S' -> S
               'T' -> T
               'V' -> V
               'W' -> W
               'Y' -> Y
               _   -> error "fromWord: cannot convert to AminoAcid."

derivingUnbox "AminoAcid"
    [t| AminoAcid -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character AminoAcid where
  toWord   = toWord
  fromWord = fromWord
