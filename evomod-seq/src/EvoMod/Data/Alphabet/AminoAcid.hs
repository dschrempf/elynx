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

Amino acid IUPAC type; normal amino acids in alphabetical order; then IUPAC
codes in alphabetical order.

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
B.................Asx.................Aspartic acid or Asparagine
X.................Xaa.................Any amino acid
Z.................Glx.................Glutamine or Glutamic acid
Additionally, I add:
-.................Gap.................No amino acid
@

-}

module EvoMod.Data.Alphabet.AminoAcid
  ( standard
  , iupac
  , gap
  , unknown
  , iupacToStandard
  ) where

import qualified Data.Map.Strict                as M
import qualified Data.Set                       as S

import           EvoMod.Data.Alphabet.Character

-- | Amino acids; alphabetical order.
standard :: S.Set Character
standard = S.fromList $ fromString "ACDEFGHIKLMNPQRSTVWY"

-- | Amino acids IUPAC code characters.
iupac :: S.Set Character
iupac = S.fromList $ fromString "BXZ-"

-- | Amino acid gap characters.
gap :: S.Set Character
gap = S.fromList $ fromString "-"

-- | Amino acid unknown characters.
unknown :: S.Set Character
unknown = S.fromList $ fromString "X"

-- | Convert IUPAC code to set of normal amino acids.
iupacToStandard :: M.Map Character [Character]
iupacToStandard = M.fromList $ map (\(k, v) -> (fromChar k, fromString v))
                  [ ('A', "A")
                  , ('C', "C")
                  , ('D', "D")
                  , ('E', "E")
                  , ('F', "F")
                  , ('G', "G")
                  , ('H', "H")
                  , ('I', "I")
                  , ('K', "K")
                  , ('L', "L")
                  , ('M', "M")
                  , ('N', "N")
                  , ('P', "P")
                  , ('Q', "Q")
                  , ('R', "R")
                  , ('S', "S")
                  , ('T', "T")
                  , ('V', "V")
                  , ('W', "W")
                  , ('Y', "Y")
                  , ('B', "DN")
                  , ('X', "ACDEFGHIKLMNPQRSTVWY")
                  , ('Z', "EQ")
                  , ('-', "")
                  ]
