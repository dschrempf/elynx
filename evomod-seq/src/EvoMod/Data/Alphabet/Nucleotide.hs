{- |
Module      :  EvoMod.Data.Nucleotide
Description :  Nucleotide related types and functions
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
Additionally, I add
-       Gap (Zero)                      -
.       Gap (Zero)                      .
@

-}

module EvoMod.Data.Alphabet.Nucleotide
  ( Nucleotide (..)
  , fromCharacter
  , standard
  , iupac
  , gap
  , unknown
  , iupacToStandard
  ) where

import qualified Data.Map.Strict                as M

import           EvoMod.Data.Alphabet.Character

-- | Nucleotides.
data Nucleotide = A | C | G | T
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

fromCharacter :: Character -> Nucleotide
fromCharacter c | toChar c == 'A' = A
                | toChar c == 'C' = C
                | toChar c == 'G' = G
                | toChar c == 'T' = T
                | otherwise       = error "fromCharacter: cannot convert Character to Nucleotide."

-- | Standard nucleotides; alphabetical order.
standard :: [Character]
standard = fromString "ACGT"

-- | Nucleotide IUPAC code characters.
iupac :: [Character]
iupac = fromString "UWSMKRYBDHVNZ-"

-- | Nucleotide gap characters.
gap :: [Character]
gap = fromString "Z-."

-- | Nucleotide unknown characters.
unknown :: [Character]
unknown = fromString "N"

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
                  , ('.', "")
                  ]
