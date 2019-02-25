{- |
Module      :  EvoMod.Data.AminoAcid
Description :  Amino acid related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

See header of 'EvoMod.Data.Alphabet'.

-}

module EvoMod.Data.Alphabet.AminoAcid
  ( AminoAcid
  , wordToAminoAcid
  , aminoAcidToWord
  ) where

import           Data.Word8                     (Word8, toUpper)

import           EvoMod.Data.Alphabet.Character
import           EvoMod.Tools.ByteString        (c2w, w2c)

-- | Amino acids type; alphabetical order.
data AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Doh, we want to read 'AminoAcid's.
wordToAminoAcid :: Word8 -> AminoAcid
wordToAminoAcid w | w' == c2w 'A' = A
                  | w' == c2w 'C' = C
                  | w' == c2w 'D' = D
                  | w' == c2w 'E' = E
                  | w' == c2w 'F' = F
                  | w' == c2w 'G' = G
                  | w' == c2w 'H' = H
                  | w' == c2w 'I' = I
                  | w' == c2w 'K' = K
                  | w' == c2w 'L' = L
                  | w' == c2w 'M' = M
                  | w' == c2w 'N' = N
                  | w' == c2w 'P' = P
                  | w' == c2w 'Q' = Q
                  | w' == c2w 'R' = R
                  | w' == c2w 'S' = S
                  | w' == c2w 'T' = T
                  | w' == c2w 'V' = V
                  | w' == c2w 'W' = W
                  | w' == c2w 'Y' = Y
                  | otherwise     = error $ "Cannot read amino acid: " ++ [w2c w] ++ "."
  where w' = toUpper w

-- | And writing is also interesting.
aminoAcidToWord :: AminoAcid-> Word8
aminoAcidToWord  A = c2w 'A'
aminoAcidToWord  C = c2w 'C'
aminoAcidToWord  D = c2w 'D'
aminoAcidToWord  E = c2w 'E'
aminoAcidToWord  F = c2w 'F'
aminoAcidToWord  G = c2w 'G'
aminoAcidToWord  H = c2w 'H'
aminoAcidToWord  I = c2w 'I'
aminoAcidToWord  K = c2w 'K'
aminoAcidToWord  L = c2w 'L'
aminoAcidToWord  M = c2w 'M'
aminoAcidToWord  N = c2w 'N'
aminoAcidToWord  P = c2w 'P'
aminoAcidToWord  Q = c2w 'Q'
aminoAcidToWord  R = c2w 'R'
aminoAcidToWord  S = c2w 'S'
aminoAcidToWord  T = c2w 'T'
aminoAcidToWord  V = c2w 'V'
aminoAcidToWord  W = c2w 'W'
aminoAcidToWord  Y = c2w 'Y'

instance Character AminoAcid where
  fromWord = wordToAminoAcid
  toWord   = aminoAcidToWord

-- AA_IUPAC
-- Amino Acid Code:  Three letter Code:  Amino Acid:
-- ----------------  ------------------  -----------
-- A.................Ala.................Alanine
-- B.................Asx.................Aspartic acid or Asparagine
-- C.................Cys.................Cysteine
-- D.................Asp.................Aspartic Acid
-- E.................Glu.................Glutamic Acid
-- F.................Phe.................Phenylalanine
-- G.................Gly.................Glycine
-- H.................His.................Histidine
-- I.................Ile.................Isoleucine
-- K.................Lys.................Lysine
-- L.................Leu.................Leucine
-- M.................Met.................Methionine
-- N.................Asn.................Asparagine
-- P.................Pro.................Proline
-- Q.................Gln.................Glutamine
-- R.................Arg.................Arginine
-- S.................Ser.................Serine
-- T.................Thr.................Threonine
-- V.................Val.................Valine
-- W.................Trp.................Tryptophan
-- X.................Xaa.................Any amino acid
-- Y.................Tyr.................Tyrosine
-- Z.................Glx.................Glutamine or Glutamic acid
