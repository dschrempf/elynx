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

Amino acids with gaps and translation stops.

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
-----------------
*                 Stp                 No amino acid
-----------------
-                 Gap                 No amino acid
.                 Gap                 No amino acid
@

-}

module EvoMod.Data.Alphabet.AminoAcidS
  ( AminoAcidS (..)
  ) where

import qualified Data.Map.Strict                as M
import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Tools.ByteString        (c2w)

-- | Amino acids.
data AminoAcidS = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
                | Stop
                | Gap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWordM :: M.Map AminoAcidS Word8
toWordM = M.fromList $ map (\(k, v) -> (k, c2w v))
  [ (A, 'A')
  , (C, 'C')
  , (D, 'D')
  , (E, 'E')
  , (F, 'F')
  , (G, 'G')
  , (H, 'H')
  , (I, 'I')
  , (K, 'K')
  , (L, 'L')
  , (M, 'M')
  , (N, 'N')
  , (P, 'P')
  , (Q, 'Q')
  , (R, 'R')
  , (S, 'S')
  , (T, 'T')
  , (V, 'V')
  , (W, 'W')
  , (Y, 'Y')
  , (Stop, '*')
  , (Gap, '-')
  ]

toWord :: AminoAcidS -> Word8
toWord = (M.!) toWordM

fromWordM :: M.Map Word8 AminoAcidS
fromWordM = M.fromList $ map (\(k, v) -> (c2w k, v))
  [ ('A', A)
  , ('C', C)
  , ('D', D)
  , ('E', E)
  , ('F', F)
  , ('G', G)
  , ('H', H)
  , ('I', I)
  , ('K', K)
  , ('L', L)
  , ('M', M)
  , ('N', N)
  , ('P', P)
  , ('Q', Q)
  , ('R', R)
  , ('S', S)
  , ('T', T)
  , ('V', V)
  , ('W', W)
  , ('Y', Y)
  , ('*', Stop)
  , ('-', Gap)
  , ('.', Gap)
  ]

fromWord :: Word8 -> AminoAcidS
fromWord = (M.!) fromWordM

derivingUnbox "AminoAcidS"
    [t| AminoAcidS -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character AminoAcidS where
  toWord   = toWord
  fromWord = fromWord
  code     = C.ProteinS

instance C.CharacterX AminoAcidS where
  gap = Gap
