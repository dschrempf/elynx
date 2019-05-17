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
A.................Ala.................Alanine
C.................Cys.................Cysteine
D.................Asp.................Aspartic Acid
E.................Glu.................Glutamic Acid
F.................Phe.................Phenylalanine
G.................Gly.................Glycine
H.................His.................Histidine
I.................Ile.................Isoleucine
K.................Lys.................Lysine
L.................Leu.................Leucine
M.................Met.................Methionine
N.................Asn.................Asparagine
P.................Pro.................Proline
Q.................Gln.................Glutamine
R.................Arg.................Arginine
S.................Ser.................Serine
T.................Thr.................Threonine
V.................Val.................Valine
W.................Trp.................Tryptophan
Y.................Tyr.................Tyrosine
@

-}

module EvoMod.Data.Alphabet.AminoAcid
  (
  ) where

import qualified Data.Map.Strict                as M
import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Tools.ByteString        (c2w)

-- | Amino acids.
data AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWordM :: M.Map AminoAcid Word8
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
  ]

toWord :: AminoAcid -> Word8
toWord = (M.!) toWordM

fromWordM :: M.Map Word8 AminoAcid
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
  ]

fromWord :: Word8 -> AminoAcid
fromWord = (M.!) fromWordM

derivingUnbox "AminoAcid"
    [t| AminoAcid -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character AminoAcid where
  toWord   = toWord
  fromWord = fromWord

-- -- | Amino acids; alphabetical order.
-- standard :: [Character]
-- standard = fromString "ACDEFGHIKLMNPQRSTVWY"

-- -- | Amino acids IUPAC code characters.
-- iupac :: [Character]
-- iupac = fromString "BXZ-"

-- -- | Amino acid gap characters.
-- gap :: [Character]
-- gap = fromString "-."

-- -- | Amino acid unknown characters.
-- unknown :: [Character]
-- unknown = fromString "X"

-- -- | When translating DNA sequences, stop codons may be encountered.
-- stop :: Character
-- stop = fromChar '*'

-- -- | Convert IUPAC code to set of normal amino acids.
-- iupacToStandard :: M.Map Character [Character]
-- iupacToStandard = M.fromList $ map (\(k, v) -> (fromChar k, fromString v))
--                   [ ('A', "A")
--                   , ('C', "C")
--                   , ('D', "D")
--                   , ('E', "E")
--                   , ('F', "F")
--                   , ('G', "G")
--                   , ('H', "H")
--                   , ('I', "I")
--                   , ('K', "K")
--                   , ('L', "L")
--                   , ('M', "M")
--                   , ('N', "N")
--                   , ('P', "P")
--                   , ('Q', "Q")
--                   , ('R', "R")
--                   , ('S', "S")
--                   , ('T', "T")
--                   , ('V', "V")
--                   , ('W', "W")
--                   , ('Y', "Y")
--                   , ('B', "DN")
--                   , ('X', "ACDEFGHIKLMNPQRSTVWY")
--                   , ('Z', "EQ")
--                   , ('-', "")
--                   , ('.', "")
--                   ]
