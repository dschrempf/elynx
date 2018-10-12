{- |
Module      :  Evol.Data.Nucleotide
Description :  Nucleotide related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

This module should be imported qualified.

-}

module Evol.Data.Nucleotide
  ( Nucleotide (..)
  , fromWord
  , toWord
  , alphabet
  , NucleotideIUPAC (..)
  , fromWordIUPAC
  , toWordIUPAC
  , alphabetIUPAC )
where

import qualified Data.Set           as S
import           Data.Word8         (Word8, toUpper)

import           Evol.Data.Alphabet
import           Evol.Tools         (c2w)

-- | Nucleotide data type. Actually, only two bits are needed, but 'Word8' is
-- the smallest available data type. One could use a 'pack' function like it is
-- done with 'ByteString's to decrease memory usage and pack a number of
-- 'Nucleotide's into one 'Word8'. By convention, I use uppercase letters.
data Nucleotide = A | C | G | T
  deriving (Show, Eq, Ord, Enum)

fromWord :: Word8 -> Nucleotide
fromWord w | w' == c2w 'A' = A
               | w' == c2w 'C' = C
               | w' == c2w 'G' = G
               | w' == c2w 'T' = T
               | otherwise     = error $ "Cannot read nucleotide: " ++ show w ++ "."
  where w' = toUpper w

toWord :: Nucleotide -> Word8
toWord A = c2w 'A'
toWord C = c2w 'C'
toWord G = c2w 'G'
toWord T = c2w 'T'

alphabet :: Alphabet
alphabet = Alphabet . S.fromList . map toWord . enumFrom $ A

-- | Nucleotides with IUPAC code.
-- A  adenosine          C  cytidine             G  guanine
-- T  thymidine          N  A/G/C/T (any)        U  uridine
-- K  G/T (keto)         S  G/C (strong)         Y  T/C (pyrimidine)
-- M  A/C (amino)        W  A/T (weak)           R  G/A (purine)
-- B  G/T/C              D  G/A/T                H  A/C/T
-- V  G/C/A              -  gap of indeterminate length
data NucleotideIUPAC = A_IUPAC | C_IUPAC | G_IUPAC | T_IUPAC
                     | N | U | K | S | Y | M | W | R | B | D | H | V | Gap
  deriving (Show, Eq, Ord, Enum)

fromWordIUPAC :: Word8 -> NucleotideIUPAC
fromWordIUPAC w | w' == c2w 'A' = A_IUPAC
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

toWordIUPAC :: NucleotideIUPAC -> Word8
toWordIUPAC A_IUPAC = c2w 'A'
toWordIUPAC C_IUPAC = c2w 'C'
toWordIUPAC G_IUPAC = c2w 'G'
toWordIUPAC T_IUPAC = c2w 'T'
toWordIUPAC N = c2w 'N'
toWordIUPAC U = c2w 'N'
toWordIUPAC K = c2w 'N'
toWordIUPAC S = c2w 'N'
toWordIUPAC Y = c2w 'N'
toWordIUPAC M = c2w 'N'
toWordIUPAC W = c2w 'N'
toWordIUPAC R = c2w 'N'
toWordIUPAC B = c2w 'N'
toWordIUPAC D = c2w 'N'
toWordIUPAC H = c2w 'N'
toWordIUPAC V = c2w 'N'
toWordIUPAC Gap = c2w '-'

alphabetIUPAC :: Alphabet
alphabetIUPAC = Alphabet . S.fromList . map toWordIUPAC . enumFrom $ A_IUPAC
