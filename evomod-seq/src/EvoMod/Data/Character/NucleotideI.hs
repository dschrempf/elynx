{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Module      :  EvoMod.Data.NucleotideI
Description :  Nucleotides with IUPAC characters
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.


See header of 'EvoMod.Data.Alphabet.Alphabet'.

Nucleotide IUPAC code. See also https://www.bioinformatics.org/sms/iupac.html or
https://en.wikipedia.org/wiki/International_Union_of_Pure_and_Applied_Chemistry.

@
Symbol  Description  Bases represented  Complement
------  -----------  -----------------  ----------
A       Adenine      A                  T
C       Cytosine        C               G
G       Guanine            G            C
T       Thymine               T         A
------  -----------  -----------------  ----------
U       Uracil                U         A
W       Weak         A        T         W
S       Strong          C  G            S
M       aMino        A  C               K
K       Keto               G  T         M
R       puRine       A     G            Y
Y       pYrimidine      C     T         R
B       not A           C  G  T         V
D       not C        A     G  T         H
H       not G        A  C     T         D
V       not T        A  C  G            B
------  -----------  -----------------  ----------
N       any          A  C  G  T         N
------  -----------  -----------------  ----------
- or .  Gap (Zero)                      -
@

-}

module EvoMod.Data.Alphabet.NucleotideI
  ( NucleotideI (..)
  ) where

import qualified Data.Map.Strict                as M
import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Tools.ByteString        (c2w)

-- | NucleotideIs.
data NucleotideI = A | C | G | T
                 | U | W | S | M | K | R | Y | B | D | H | V
                 | N
                 | Gap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWordM :: M.Map NucleotideI Word8
toWordM = M.fromList $ map (\(k, v) -> (k, c2w v))
  [ (A, 'A')
  , (C, 'C')
  , (G, 'G')
  , (T, 'T')
  , (U, 'U')
  , (W, 'W')
  , (S, 'S')
  , (M, 'M')
  , (K, 'K')
  , (R, 'R')
  , (Y, 'Y')
  , (B, 'B')
  , (D, 'D')
  , (H, 'H')
  , (V, 'V')
  , (N, 'N')
  , (Gap, '-')
  ]

toWord :: NucleotideI -> Word8
toWord = (M.!) toWordM

fromWordM :: M.Map Word8 NucleotideI
fromWordM = M.fromList $ map (\(k, v) -> (c2w k, v))
  [ ('A', A)
  , ('C', C)
  , ('G', G)
  , ('T', T)
  , ('U', U)
  , ('W', W)
  , ('S', S)
  , ('M', M)
  , ('K', K)
  , ('R', R)
  , ('Y', Y)
  , ('B', B)
  , ('D', D)
  , ('H', H)
  , ('V', V)
  , ('N', N)
  , ('-', Gap)
  , ('.', Gap)
  ]

fromWord :: Word8 -> NucleotideI
fromWord = (M.!) fromWordM

derivingUnbox "NucleotideI"
    [t| NucleotideI -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character NucleotideI where
  toWord   = toWord
  fromWord = fromWord
  code     = C.DNAI

toStandardM :: M.Map NucleotideI [NucleotideI]
toStandardM = M.fromList
  [
    (A, [A])
  , (C, [C])
  , (G, [G])
  , (T, [T])
  , (U, [T])
  , (W, [A, T])
  , (S, [G, C])
  , (M, [A, C])
  , (K, [G, T])
  , (R, [A, G])
  , (Y, [C, T])
  , (B, [C, G, T])
  , (D, [A, G, T])
  , (H, [A, C, T])
  , (V, [A, C, G])
  , (N, [A, C, G, T])
  , (Gap, [])
  ]

instance C.CharacterX NucleotideI where
  gap        = Gap

instance C.CharacterI NucleotideI where
  -- XXX: Should the gap be in here?
  iupac = [U, W, S, N, K, R, Y, B, D, H, V, N, Gap]
  toStandard = (M.!) toStandardM
