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

Extended amino acid with gaps and unknowns. See also
https://www.bioinformatics.org/sms/iupac.html or
https://en.wikipedia.org/wiki/International_Union_of_Pure_and_Applied_Chemistry.

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
X                 Xaa                 Any amino acid
-----------------
-                 Gap                 No amino acid
.                 Gap                 No amino acid
@

-}

module EvoMod.Data.Alphabet.AminoAcidX
  (
  ) where

import qualified Data.Map.Strict                as M
import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Tools.ByteString        (c2w)

-- | Amino acids.
data AminoAcidX = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
                | X
                | Gap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWordM :: M.Map AminoAcidX Word8
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
  , (X, 'X')
  , (Gap, '-')
  ]

toWord :: AminoAcidX -> Word8
toWord = (M.!) toWordM

fromWordM :: M.Map Word8 AminoAcidX
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
  , ('X', X)
  , ('-', Gap)
  , ('.', Gap)                  -- Support dot gap character.
  ]

fromWord :: Word8 -> AminoAcidX
fromWord = (M.!) fromWordM

derivingUnbox "AminoAcidX"
    [t| AminoAcidX -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character AminoAcidX where
  toWord   = toWord
  fromWord = fromWord

toStandardM :: M.Map AminoAcidX [AminoAcidX]
toStandardM = M.fromList
  [ (A, [A])
  , (C, [C])
  , (D, [D])
  , (E, [E])
  , (F, [F])
  , (G, [G])
  , (H, [H])
  , (I, [I])
  , (K, [K])
  , (L, [L])
  , (M, [M])
  , (N, [N])
  , (P, [P])
  , (Q, [Q])
  , (R, [R])
  , (S, [S])
  , (T, [T])
  , (V, [V])
  , (W, [W])
  , (Y, [Y])
  , (X, [A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y])
  , (Gap, [])
  ]

instance C.CharacterX AminoAcidX where
  unknown    = X
  gap        = Gap
  toStandard = (M.!) toStandardM
