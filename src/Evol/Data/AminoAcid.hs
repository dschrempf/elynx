{- |
Module      :  Evol.Data.AminoAcid
Description :  Amino acid related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

This module should be imported qualified.

-}

module Evol.Data.AminoAcid
  ( AminoAcid
  , fromWord
  , toWord
  , alphabet
  ) where

import qualified Data.Set                     as S
import           Data.Word8                   (Word8, toUpper)

import           Evol.Data.Alphabet
import           Evol.Tools                   (c2w)

-- | Amino acids.
data AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y
  deriving (Show, Eq, Ord, Enum)

fromWord :: Word8 -> AminoAcid
fromWord w | w' == c2w 'A' = A
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
           | otherwise     = error $ "Cannot read nucleotide: " ++ show w ++ "."
  where w' = toUpper w

toWord :: AminoAcid-> Word8
toWord  A = c2w 'A'
toWord  C = c2w 'C'
toWord  D = c2w 'D'
toWord  E = c2w 'E'
toWord  F = c2w 'F'
toWord  G = c2w 'G'
toWord  H = c2w 'H'
toWord  I = c2w 'I'
toWord  K = c2w 'K'
toWord  L = c2w 'L'
toWord  M = c2w 'M'
toWord  N = c2w 'N'
toWord  P = c2w 'P'
toWord  Q = c2w 'Q'
toWord  R = c2w 'R'
toWord  S = c2w 'S'
toWord  T = c2w 'T'
toWord  V = c2w 'V'
toWord  W = c2w 'W'
toWord  Y = c2w 'Y'

alphabet :: Alphabet
alphabet = Alphabet . S.fromList . map toWord . enumFrom $ A

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
