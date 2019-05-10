{- |
Module      :  EvoMod.Data.Nucleotide
Description :  Nucleotide related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.


See header of 'EvoMod.Data.Alphabet'.

Nucleotide IUPAC code (ordering according to list on Wikipedia).

@
Symbol  Description  Bases represented  Complement
------  -----------  -----------------  ----------
A       Adenine      A                  T
C       Cytosine        C               G
G       Guanine            G            C
T       Thymine               T         A
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
N       any          A  C  G  T         N
Z       Zero                            Z
Also used:
-       Gap (Zero)                      -
@

-}

module EvoMod.Data.Alphabet.Nucleotide
  ( standard
  , iupac
  , gap
  , unknown
  , iupacToStandard
  ) where

import qualified Data.Map.Strict                as M
import qualified Data.Set                       as S

import           EvoMod.Data.Alphabet.Character

-- | Standard nucleotides; alphabetical order.
standard :: S.Set Character
standard = S.fromList $ fromString "ACGT"

-- | Nucleotide IUPAC code characters.
iupac :: S.Set Character
iupac = S.fromList $ fromString "UWSMKRYBDHVNZ-"

-- | Nucleotide gap characters.
gap :: S.Set Character
gap = S.fromList $ fromString "Z-"

-- | Nucleotide unknown characters.
unknown :: S.Set Character
unknown = S.fromList $ fromString "N"

-- | Convert IUPAC code to set of normal nucleotides.
iupacToStandard :: M.Map Character [Character]
iupacToStandard = M.fromList $ map (\(k, v) -> (fromChar k, fromString v))
                  [ ('A', "A")
                  , ('C', "C")
                  , ('G', "G")
                  , ('T', "T")
                  , ('U', "T")
                  , ('W', "AT")
                  , ('S', "GC")
                  , ('M', "AC")
                  , ('K', "GT")
                  , ('R', "AG")
                  , ('Y', "CT")
                  , ('B', "CGT")
                  , ('D', "AGT")
                  , ('H', "ACT")
                  , ('V', "ACG")
                  , ('N', "ACGT")
                  , ('Z', "")
                  , ('-', "")
                  ]
