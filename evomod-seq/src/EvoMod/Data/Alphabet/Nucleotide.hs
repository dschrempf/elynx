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

-}

module EvoMod.Data.Alphabet.Nucleotide
  ( nucleotides
  , nucleotidesIUPAC
  , isGapOrUnknownNucleotide
  -- , fromIUPACNucleotide
  ) where

import           EvoMod.Data.Alphabet.Character
-- import           EvoMod.Tools.ByteString        (c2w, w2c)

nucleotides :: [Character]
nucleotides = map fromChar "ACGT"

-- | Nucleotide IUPAC code (ordering according to list on Wikipedia).
--
-- @
-- Symbol  Description  Bases represented  Complement
-- ------  -----------  -----------------  ----------
-- A       Adenine      A                  T
-- C       Cytosine        C               G
-- G       Guanine            G            C
-- T       Thymine               T         A
-- U       Uracil                U         A
-- W       Weak         A        T         W
-- S       Strong          C  G            S
-- M       aMino        A  C               K
-- K       Keto               G  T         M
-- R       puRine       A     G            Y
-- Y       pYrimidine      C     T         R
-- B       not A           C  G  T         V
-- D       not C        A     G  T         H
-- H       not G        A  C     T         D
-- V       not T        A  C  G            B
-- N       any          A  C  G  T         N
-- Z       Zero                            Z
-- Additionall, I add:
-- -       Gap (same as N)                 -
-- @
nucleotidesIUPAC :: [Character]
nucleotidesIUPAC = map fromChar "ACGTUWSMKRYBDHVNZ-"

isGapOrUnknownNucleotide :: Character -> Bool
isGapOrUnknownNucleotide char | char == fromChar 'N' = True
                              | char == fromChar '-' = True
                              | otherwise            = False

-- -- TODO.
-- -- | Convert IUPAC code to set of normal nucleotides.
-- fromIUPACNucleotide :: NucleotideIUPAC -> [Nucleotide]
-- fromIUPACNucleotide A_IUPAC = [A]
-- fromIUPACNucleotide C_IUPAC = [C]
-- fromIUPACNucleotide G_IUPAC = [G]
-- fromIUPACNucleotide T_IUPAC = [T]
-- fromIUPACNucleotide U       = [T]
-- fromIUPACNucleotide W       = [A, T]
-- fromIUPACNucleotide S       = [G, C]
-- fromIUPACNucleotide M       = [A, C]
-- fromIUPACNucleotide K       = [G, T]
-- fromIUPACNucleotide R       = [A, G]
-- fromIUPACNucleotide Y       = [C, T]
-- fromIUPACNucleotide B       = [C, G, T]
-- fromIUPACNucleotide D       = [A, G, T]
-- fromIUPACNucleotide H       = [A, C, T]
-- fromIUPACNucleotide V       = [A, C, G]
-- fromIUPACNucleotide N       = [A, C, G, T]
-- fromIUPACNucleotide Z       = []
-- fromIUPACNucleotide Gap     = [A, C, G, T]

-- module EvoMod.Data.Alphabet.Nucleotide
--   ( Nucleotide (..)
--   , wordToNucleotide
--   , nucleotideToWord
--   , NucleotideIUPAC (..)
--   , wordToNucleotideIUPAC
--   , nucleotideIUPACToWord
--   , fromIUPACNucleotide
--   ) where

-- import           Data.Word8                     (Word8, toUpper)

-- import           EvoMod.Data.Alphabet.Character
-- import           EvoMod.Tools.ByteString        (c2w, w2c)
-- -- | Nucleotide data type. Actually, only two bits are needed, but 'Word8' is
-- -- the smallest available data type. One could use a /pack/ function like it is
-- -- done with /ByteString/s to decrease memory usage and pack a number of
-- -- 'Nucleotide's into one 'Word8'. By convention, I use uppercase letters.
-- data Nucleotide = A | C | G | T
--   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- -- | Convert a 'Word8' to a 'Nucleotide'.
-- wordToNucleotide :: Word8 -> Nucleotide
-- wordToNucleotide w | w' == c2w 'A' = A
--                    | w' == c2w 'C' = C
--                    | w' == c2w 'G' = G
--                    | w' == c2w 'T' = T
--                    | otherwise     = error $ "Cannot read nucleotide: " ++ [w2c w] ++ "."
--   where w' = toUpper w

-- -- | Convert 'Nucleotide' to 'Word8'.
-- nucleotideToWord :: Nucleotide -> Word8
-- nucleotideToWord A = c2w 'A'
-- nucleotideToWord C = c2w 'C'
-- nucleotideToWord G = c2w 'G'
-- nucleotideToWord T = c2w 'T'

-- instance Character Nucleotide where
--   fromWord         = wordToNucleotide
--   toWord           = nucleotideToWord
--   isStandard _     = True
--   isGapOrUnknown _ = False

-- -- | Nucleotides with IUPAC code (ordering according to list on Wikipedia).
-- --
-- -- Nucleotide IUPAC code.
-- -- Symbol  Description  Bases represented  Complement
-- -- ------  -----------  -----------------  ----------
-- -- A       Adenine      A                  T
-- -- C       Cytosine        C               G
-- -- G       Guanine            G            C
-- -- T       Thymine               T         A
-- -- U       Uracil                U         A
-- -- W       Weak         A        T         W
-- -- S       Strong          C  G            S
-- -- M       aMino        A  C               K
-- -- K       Keto               G  T         M
-- -- R       puRine       A     G            Y
-- -- Y       pYrimidine      C     T         R
-- -- B       not A           C  G  T         V
-- -- D       not C        A     G  T         H
-- -- H       not G        A  C     T         D
-- -- V       not T        A  C  G            B
-- -- N       any          A  C  G  T         N
-- -- Z       Zero                            Z
-- --
-- -- Additionall, I add:
-- -- -       Gap (same as N)                 -
-- data NucleotideIUPAC = A_IUPAC | C_IUPAC | G_IUPAC | T_IUPAC
--                      | U | W | S | M | K | R | Y | B | D | H | V | N | Z | Gap
--   deriving (Show, Eq, Ord, Enum, Bounded)

-- -- | So that we can read 'EvoMod.Data.Fasta' files.
-- wordToNucleotideIUPAC :: Word8 -> NucleotideIUPAC
-- wordToNucleotideIUPAC w | w' == c2w 'A' = A_IUPAC
--                         | w' == c2w 'C' = C_IUPAC
--                         | w' == c2w 'G' = G_IUPAC
--                         | w' == c2w 'T' = T_IUPAC
--                         | w' == c2w 'U' = U
--                         | w' == c2w 'W' = W
--                         | w' == c2w 'S' = S
--                         | w' == c2w 'M' = M
--                         | w' == c2w 'K' = K
--                         | w' == c2w 'R' = R
--                         | w' == c2w 'Y' = Y
--                         | w' == c2w 'B' = B
--                         | w' == c2w 'D' = D
--                         | w' == c2w 'H' = H
--                         | w' == c2w 'V' = V
--                         | w' == c2w 'N' = N
--                         | w' == c2w 'Z' = Z
--                         | w' == c2w '-' = Gap
--                         | otherwise     = error $ "Cannot read nucleotide IUPAC code: " ++ [w2c w] ++ "."
--   where w' = toUpper w

-- -- | So that we can write 'NucleotideIUPAC'.
-- nucleotideIUPACToWord :: NucleotideIUPAC -> Word8
-- nucleotideIUPACToWord A_IUPAC = c2w 'A'
-- nucleotideIUPACToWord C_IUPAC = c2w 'C'
-- nucleotideIUPACToWord G_IUPAC = c2w 'G'
-- nucleotideIUPACToWord T_IUPAC = c2w 'T'
-- nucleotideIUPACToWord U       = c2w 'U'
-- nucleotideIUPACToWord W       = c2w 'W'
-- nucleotideIUPACToWord S       = c2w 'S'
-- nucleotideIUPACToWord M       = c2w 'M'
-- nucleotideIUPACToWord K       = c2w 'K'
-- nucleotideIUPACToWord R       = c2w 'R'
-- nucleotideIUPACToWord Y       = c2w 'Y'
-- nucleotideIUPACToWord B       = c2w 'B'
-- nucleotideIUPACToWord D       = c2w 'D'
-- nucleotideIUPACToWord H       = c2w 'H'
-- nucleotideIUPACToWord V       = c2w 'V'
-- nucleotideIUPACToWord N       = c2w 'N'
-- nucleotideIUPACToWord Z       = c2w 'Z'
-- nucleotideIUPACToWord Gap     = c2w '-'

-- nucleotideIUPACIsStandard :: NucleotideIUPAC -> Bool
-- nucleotideIUPACIsStandard A_IUPAC = True
-- nucleotideIUPACIsStandard C_IUPAC = True
-- nucleotideIUPACIsStandard G_IUPAC = True
-- nucleotideIUPACIsStandard T_IUPAC = True
-- nucleotideIUPACIsStandard _       = False

-- nucleotideIUPACIsGapOrUnknown :: NucleotideIUPAC -> Bool
-- nucleotideIUPACIsGapOrUnknown N   = True
-- nucleotideIUPACIsGapOrUnknown Gap = True
-- nucleotideIUPACIsGapOrUnknown _   = False

-- instance Character NucleotideIUPAC where
--   fromWord       = wordToNucleotideIUPAC
--   toWord         = nucleotideIUPACToWord
--   isStandard     = nucleotideIUPACIsStandard
--   isGapOrUnknown = nucleotideIUPACIsGapOrUnknown

-- -- | Convert IUPAC code to set of normal nucleotides.
-- fromIUPACNucleotide :: NucleotideIUPAC -> [Nucleotide]
-- fromIUPACNucleotide A_IUPAC = [A]
-- fromIUPACNucleotide C_IUPAC = [C]
-- fromIUPACNucleotide G_IUPAC = [G]
-- fromIUPACNucleotide T_IUPAC = [T]
-- fromIUPACNucleotide U       = [T]
-- fromIUPACNucleotide W       = [A, T]
-- fromIUPACNucleotide S       = [G, C]
-- fromIUPACNucleotide M       = [A, C]
-- fromIUPACNucleotide K       = [G, T]
-- fromIUPACNucleotide R       = [A, G]
-- fromIUPACNucleotide Y       = [C, T]
-- fromIUPACNucleotide B       = [C, G, T]
-- fromIUPACNucleotide D       = [A, G, T]
-- fromIUPACNucleotide H       = [A, C, T]
-- fromIUPACNucleotide V       = [A, C, G]
-- fromIUPACNucleotide N       = [A, C, G, T]
-- fromIUPACNucleotide Z       = []
-- fromIUPACNucleotide Gap     = [A, C, G, T]
