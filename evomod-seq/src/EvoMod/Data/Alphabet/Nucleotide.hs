{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

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
  (
  --   standard
  -- , iupac
  -- , gap
  -- , unknown
  -- , iupacToStandard
  ) where

-- import qualified Data.Map.Strict                as M
import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Tools.ByteString        (c2w, w2c)

-- | Nucleotides.
data Nucleotide = A | C | G | T
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

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
               _   -> error "fromWord: cannot convert to Nucleotide."

derivingUnbox "Nucleotide"
    [t| Nucleotide -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character Nucleotide where
  toWord   = toWord
  fromWord = fromWord

-- -- | Standard nucleotides; alphabetical order.
-- standard :: [Character]
-- standard = fromString "ACGT"

-- -- | Nucleotide IUPAC code characters.
-- iupac :: [Character]
-- iupac = fromString "UWSMKRYBDHVNZ-"

-- -- | Nucleotide gap characters.
-- gap :: [Character]
-- gap = fromString "Z-."

-- -- | Nucleotide unknown characters.
-- unknown :: [Character]
-- unknown = fromString "N"

-- -- | Convert IUPAC code to set of normal nucleotides.
-- iupacToStandard :: M.Map Character [Character]
-- iupacToStandard = M.fromList $ map (\(k, v) -> (fromChar k, fromString v))
--                   [ ('A', "A")
--                   , ('C', "C")
--                   , ('G', "G")
--                   , ('T', "T")
--                   , ('U', "T")
--                   , ('W', "AT")
--                   , ('S', "GC")
--                   , ('M', "AC")
--                   , ('K', "GT")
--                   , ('R', "AG")
--                   , ('Y', "CT")
--                   , ('B', "CGT")
--                   , ('D', "AGT")
--                   , ('H', "ACT")
--                   , ('V', "ACG")
--                   , ('N', "ACGT")
--                   , ('Z', "")
--                   , ('-', "")
--                   , ('.', "")
--                   ]
