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

Amino acid IUPAC code. See also https://www.bioinformatics.org/sms/iupac.html or
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
J                                     Leucine or Isoleucine
B                 Asx                 Aspartic acid or Asparagine
Z                 Glx                 Glutamine or Glutamic acid
-----------------
X                 Xaa                 Any amino acid
-----------------
*                 Stp                 No amino acid
-----------------
-                 Gap                 No amino acid
.                 Gap                 No amino acid
@

-}

module EvoMod.Data.Alphabet.AminoAcidI
  ( AminoAcidI (..)
  ) where

import qualified Data.Map.Strict                as M
import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Tools.ByteString        (c2w)

-- | Amino acids.
data AminoAcidI = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
                | J | B | Z
                | X
                | Stop
                | Gap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- toWordM :: M.Map AminoAcidI Word8
-- toWordM = M.fromList $ map (\(k, v) -> (k, c2w v))
--   [ (A,    'A')
--   , (C,    'C')
--   , (D,    'D')
--   , (E,    'E')
--   , (F,    'F')
--   , (G,    'G')
--   , (H,    'H')
--   , (I,    'I')
--   , (K,    'K')
--   , (L,    'L')
--   , (M,    'M')
--   , (N,    'N')
--   , (P,    'P')
--   , (Q,    'Q')
--   , (R,    'R')
--   , (S,    'S')
--   , (T,    'T')
--   , (V,    'V')
--   , (W,    'W')
--   , (Y,    'Y')
--   , (J,    'J')
--   , (B,    'B')
--   , (Z,    'Z')
--   , (X,    'X')
--   , (Stop, '*')
--   , (Gap,  '-')
--   ]

-- toWord :: AminoAcidI -> Word8
-- toWord = (M.!) toWordM

toWord :: AminoAcidI -> Word8
toWord A    = c2w 'A'
toWord C    = c2w 'C'
toWord D    = c2w 'D'
toWord E    = c2w 'E'
toWord F    = c2w 'F'
toWord G    = c2w 'G'
toWord H    = c2w 'H'
toWord I    = c2w 'I'
toWord K    = c2w 'K'
toWord L    = c2w 'L'
toWord M    = c2w 'M'
toWord N    = c2w 'N'
toWord P    = c2w 'P'
toWord Q    = c2w 'Q'
toWord R    = c2w 'R'
toWord S    = c2w 'S'
toWord T    = c2w 'T'
toWord V    = c2w 'V'
toWord W    = c2w 'W'
toWord Y    = c2w 'Y'
toWord J    = c2w 'J'
toWord B    = c2w 'B'
toWord Z    = c2w 'Z'
toWord X    = c2w 'X'
toWord Stop = c2w '*'
toWord Gap  = c2w '-'

-- fromWordM :: M.Map Word8 AminoAcidI
-- fromWordM = M.fromList $ map (\(k, v) -> (c2w k, v))
--   [ ('A', A)
--   , ('C', C)
--   , ('D', D)
--   , ('E', E)
--   , ('F', F)
--   , ('G', G)
--   , ('H', H)
--   , ('I', I)
--   , ('K', K)
--   , ('L', L)
--   , ('M', M)
--   , ('N', N)
--   , ('P', P)
--   , ('Q', Q)
--   , ('R', R)
--   , ('S', S)
--   , ('T', T)
--   , ('V', V)
--   , ('W', W)
--   , ('Y', Y)
--   , ('J', J)
--   , ('B', B)
--   , ('Z', Z)
--   , ('X', X)
--   , ('*', Stop)
--   , ('-', Gap)
--   , ('.', Gap)                  -- Support dot gap character.
--   ]

-- fromWord :: Word8 -> AminoAcidI
-- fromWord = (M.!) fromWordM

fromWord :: Word8 -> AminoAcidI
fromWord w | w == c2w 'A' = A
           | w == c2w 'C' = C
           | w == c2w 'D' = D
           | w == c2w 'E' = E
           | w == c2w 'F' = F
           | w == c2w 'G' = G
           | w == c2w 'H' = H
           | w == c2w 'I' = I
           | w == c2w 'K' = K
           | w == c2w 'L' = L
           | w == c2w 'M' = M
           | w == c2w 'N' = N
           | w == c2w 'P' = P
           | w == c2w 'Q' = Q
           | w == c2w 'R' = R
           | w == c2w 'S' = S
           | w == c2w 'T' = T
           | w == c2w 'V' = V
           | w == c2w 'W' = W
           | w == c2w 'Y' = Y
           | w == c2w 'J' = J
           | w == c2w 'B' = B
           | w == c2w 'Z' = Z
           | w == c2w 'X' = X
           | w == c2w '*' = S
           | w == c2w '-' = Gap
           | w == c2w '.' = Gap
           | otherwise = error "fromWord: cannot convert Word8 to AminoAcidI"

derivingUnbox "AminoAcidI"
    [t| AminoAcidI -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character AminoAcidI where
  toWord   = toWord
  fromWord = fromWord
  code     = C.ProteinI

instance C.CharacterX AminoAcidI where
  gap        = Gap

toStandardM :: M.Map AminoAcidI [AminoAcidI]
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
  , (J, [L, I])
  , (B, [D, N])
  , (Z, [E, Q])
  , (X, [A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y])
  , (Gap, [])
  ]

instance C.CharacterI AminoAcidI where
  -- XXX: Should the gap be in here?
  iupac = [J, B, Z, X, Gap]
  toStandard = (M.!) toStandardM
