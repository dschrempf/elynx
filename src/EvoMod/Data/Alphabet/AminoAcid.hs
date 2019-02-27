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
  , AminoAcidIUPAC
  , wordToAminoAcidIUPAC
  , aminoAcidIUPACToWord
  , fromIUPACAminoAcid
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

-- | Amino acids IUPAC type; normal amino acids in alphabetical order; then
-- IUPAC codes in alphabetical order.
--
-- Amino acid IUPAC code.
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
-- Additionally, I add:
-- - Gap (same as X)
data AminoAcidIUPAC = A_IUPAC | C_IUPAC | D_IUPAC | E_IUPAC
                    | F_IUPAC | G_IUPAC | H_IUPAC | I_IUPAC
                    | K_IUPAC | L_IUPAC | M_IUPAC | N_IUPAC
                    | P_IUPAC | Q_IUPAC | R_IUPAC | S_IUPAC
                    | T_IUPAC | V_IUPAC | W_IUPAC | Y_IUPAC
                    | B | X | Z | Gap
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Well, ... ... .
wordToAminoAcidIUPAC :: Word8 -> AminoAcidIUPAC
wordToAminoAcidIUPAC w | w' == c2w 'A' = A_IUPAC
                       | w' == c2w 'C' = C_IUPAC
                       | w' == c2w 'D' = D_IUPAC
                       | w' == c2w 'E' = E_IUPAC
                       | w' == c2w 'F' = F_IUPAC
                       | w' == c2w 'G' = G_IUPAC
                       | w' == c2w 'H' = H_IUPAC
                       | w' == c2w 'I' = I_IUPAC
                       | w' == c2w 'K' = K_IUPAC
                       | w' == c2w 'L' = L_IUPAC
                       | w' == c2w 'M' = M_IUPAC
                       | w' == c2w 'N' = N_IUPAC
                       | w' == c2w 'P' = P_IUPAC
                       | w' == c2w 'Q' = Q_IUPAC
                       | w' == c2w 'R' = R_IUPAC
                       | w' == c2w 'S' = S_IUPAC
                       | w' == c2w 'T' = T_IUPAC
                       | w' == c2w 'V' = V_IUPAC
                       | w' == c2w 'W' = W_IUPAC
                       | w' == c2w 'Y' = Y_IUPAC
                       | w' == c2w 'B' = B
                       | w' == c2w 'X' = X
                       | w' == c2w 'Z' = Z
                       | w' == c2w '-' = Gap
                       | otherwise     = error $ "Cannot read amino acid IUPAC code: " ++ [w2c w] ++ "."
  where w' = toUpper w

-- | And writing is also interesting.
aminoAcidIUPACToWord :: AminoAcidIUPAC-> Word8
aminoAcidIUPACToWord A_IUPAC = c2w 'A'
aminoAcidIUPACToWord C_IUPAC = c2w 'C'
aminoAcidIUPACToWord D_IUPAC = c2w 'D'
aminoAcidIUPACToWord E_IUPAC = c2w 'E'
aminoAcidIUPACToWord F_IUPAC = c2w 'F'
aminoAcidIUPACToWord G_IUPAC = c2w 'G'
aminoAcidIUPACToWord H_IUPAC = c2w 'H'
aminoAcidIUPACToWord I_IUPAC = c2w 'I'
aminoAcidIUPACToWord K_IUPAC = c2w 'K'
aminoAcidIUPACToWord L_IUPAC = c2w 'L'
aminoAcidIUPACToWord M_IUPAC = c2w 'M'
aminoAcidIUPACToWord N_IUPAC = c2w 'N'
aminoAcidIUPACToWord P_IUPAC = c2w 'P'
aminoAcidIUPACToWord Q_IUPAC = c2w 'Q'
aminoAcidIUPACToWord R_IUPAC = c2w 'R'
aminoAcidIUPACToWord S_IUPAC = c2w 'S'
aminoAcidIUPACToWord T_IUPAC = c2w 'T'
aminoAcidIUPACToWord V_IUPAC = c2w 'V'
aminoAcidIUPACToWord W_IUPAC = c2w 'W'
aminoAcidIUPACToWord Y_IUPAC = c2w 'Y'
aminoAcidIUPACToWord B       = c2w 'B'
aminoAcidIUPACToWord X       = c2w 'X'
aminoAcidIUPACToWord Z       = c2w 'Z'
aminoAcidIUPACToWord Gap     = c2w '-'

instance Character AminoAcidIUPAC where
  fromWord = wordToAminoAcidIUPAC
  toWord   = aminoAcidIUPACToWord

-- | Convert IUPAC code to set of normal amino acids.
fromIUPACAminoAcid :: AminoAcidIUPAC -> [AminoAcid]
fromIUPACAminoAcid A_IUPAC = [A]
fromIUPACAminoAcid C_IUPAC = [C]
fromIUPACAminoAcid D_IUPAC = [D]
fromIUPACAminoAcid E_IUPAC = [E]
fromIUPACAminoAcid F_IUPAC = [F]
fromIUPACAminoAcid G_IUPAC = [G]
fromIUPACAminoAcid H_IUPAC = [H]
fromIUPACAminoAcid I_IUPAC = [I]
fromIUPACAminoAcid K_IUPAC = [K]
fromIUPACAminoAcid L_IUPAC = [L]
fromIUPACAminoAcid M_IUPAC = [M]
fromIUPACAminoAcid N_IUPAC = [N]
fromIUPACAminoAcid P_IUPAC = [P]
fromIUPACAminoAcid Q_IUPAC = [Q]
fromIUPACAminoAcid R_IUPAC = [R]
fromIUPACAminoAcid S_IUPAC = [S]
fromIUPACAminoAcid T_IUPAC = [T]
fromIUPACAminoAcid V_IUPAC = [V]
fromIUPACAminoAcid W_IUPAC = [W]
fromIUPACAminoAcid Y_IUPAC = [Y]
fromIUPACAminoAcid B       = [D, N]
fromIUPACAminoAcid X       = [A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y]
fromIUPACAminoAcid Z       = [E, Q]
-- fromIUPACAminoAcid Gap     = [A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y]
fromIUPACAminoAcid Gap     = []
