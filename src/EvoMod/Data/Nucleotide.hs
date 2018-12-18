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

module EvoMod.Data.Nucleotide
  ( Nucleotide (..)
  , wordToNucleotide
  , nucleotideToWord
  , NucleotideIUPAC (..)
  , wordToNucleotideIUPAC
  , nucleotideIUPACToWord )
where

import           Data.Word8          (Word8, toUpper)

import           EvoMod.Data.Character
import           EvoMod.Tools          (c2w)

-- | Nucleotide data type. Actually, only two bits are needed, but 'Word8' is
-- the smallest available data type. One could use a 'pack' function like it is
-- done with 'ByteString's to decrease memory usage and pack a number of
-- 'Nucleotide's into one 'Word8'. By convention, I use uppercase letters.
data Nucleotide = A | C | G | T
  deriving (Show, Eq, Ord, Enum, Bounded)

wordToNucleotide :: Word8 -> Nucleotide
wordToNucleotide w | w' == c2w 'A' = A
                   | w' == c2w 'C' = C
                   | w' == c2w 'G' = G
                   | w' == c2w 'T' = T
                   | otherwise     = error $ "Cannot read nucleotide: " ++ show w ++ "."
  where w' = toUpper w

nucleotideToWord :: Nucleotide -> Word8
nucleotideToWord A = c2w 'A'
nucleotideToWord C = c2w 'C'
nucleotideToWord G = c2w 'G'
nucleotideToWord T = c2w 'T'

instance Character Nucleotide where
  fromWord = wordToNucleotide
  toWord   = nucleotideToWord

-- | Nucleotides with IUPAC code.
-- A  adenosine          C  cytidine             G  guanine
-- T  thymidine          N  A/G/C/T (any)        U  uridine
-- K  G/T (keto)         S  G/C (strong)         Y  T/C (pyrimidine)
-- M  A/C (amino)        W  A/T (weak)           R  G/A (purine)
-- B  G/T/C              D  G/A/T                H  A/C/T
-- V  G/C/A              -  gap of indeterminate length
data NucleotideIUPAC = A_IUPAC | C_IUPAC | G_IUPAC | T_IUPAC
                     | N | U | K | S | Y | M | W | R | B | D | H | V | Gap
  deriving (Show, Eq, Ord, Enum, Bounded)

wordToNucleotideIUPAC :: Word8 -> NucleotideIUPAC
wordToNucleotideIUPAC w | w' == c2w 'A' = A_IUPAC
                        | w' == c2w 'C' = C_IUPAC
                        | w' == c2w 'G' = G_IUPAC
                        | w' == c2w 'T' = T_IUPAC
                        | w' == c2w 'N' = N
                        | w' == c2w 'U' = U
                        | w' == c2w 'K' = K
                        | w' == c2w 'S' = S
                        | w' == c2w 'Y' = Y
                        | w' == c2w 'M' = M
                        | w' == c2w 'W' = W
                        | w' == c2w 'R' = R
                        | w' == c2w 'B' = B
                        | w' == c2w 'D' = D
                        | w' == c2w 'H' = H
                        | w' == c2w 'V' = V
                        | w' == c2w '-' = Gap
                        | otherwise     = error $ "Cannot read nucleotide IUPAC code: " ++ show w ++ "."
  where w' = toUpper w

nucleotideIUPACToWord :: NucleotideIUPAC -> Word8
nucleotideIUPACToWord A_IUPAC = c2w 'A'
nucleotideIUPACToWord C_IUPAC = c2w 'C'
nucleotideIUPACToWord G_IUPAC = c2w 'G'
nucleotideIUPACToWord T_IUPAC = c2w 'T'
nucleotideIUPACToWord N       = c2w 'N'
nucleotideIUPACToWord U       = c2w 'U'
nucleotideIUPACToWord K       = c2w 'K'
nucleotideIUPACToWord S       = c2w 'S'
nucleotideIUPACToWord Y       = c2w 'Y'
nucleotideIUPACToWord M       = c2w 'M'
nucleotideIUPACToWord W       = c2w 'W'
nucleotideIUPACToWord R       = c2w 'R'
nucleotideIUPACToWord B       = c2w 'B'
nucleotideIUPACToWord D       = c2w 'D'
nucleotideIUPACToWord H       = c2w 'H'
nucleotideIUPACToWord V       = c2w 'V'
nucleotideIUPACToWord Gap     = c2w '-'

instance Character NucleotideIUPAC where
  fromWord = wordToNucleotideIUPAC
  toWord   = nucleotideIUPACToWord
