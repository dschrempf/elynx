{- |
Module      :  EvoMod.Data.Alphabet.AlphabetNew
Description :  Alphabets store hereditary information
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May 10 11:10:32 2019.

New implementation.

-}

module EvoMod.Data.Alphabet.AlphabetNew
  (
    Code (..)
  , codeNameVerbose
  , Alphabet (..)
  , alphabet
  ) where

import qualified Data.MemoCombinators    as Memo
import qualified Data.Set                as S
import qualified Data.Vector.Unboxed     as V
import           Data.Word8

import           EvoMod.Tools.ByteString

-- | A set of characters forms an 'Alphabet'. At the moment, 'Word8' is used,
-- since none of the alphabets has more than 255 characters.
type Character = Word8

-- One could add extended DNA or Protein here (standard characters plus gaps and
-- unknowns).
-- | The used genetic code. Could include Protein_IUPAC, CountsFile for
-- population data and so on.
data Code = DNA | DNAIUPAC | Protein | ProteinIUPAC
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Verbose version of code name.
codeNameVerbose :: Code -> String
codeNameVerbose DNA          = show DNA ++ " (nucleotides)"
codeNameVerbose DNAIUPAC     = show DNAIUPAC ++ " (nucleotides including IUPAC codes)"
codeNameVerbose Protein      = show Protein ++ " (amino acids)"
codeNameVerbose ProteinIUPAC = show ProteinIUPAC ++ " (amino acids including IUPAC codes)"

-- | An alphabet is a vector of characters with a specific order.
newtype Alphabet = Alphabet { fromAlphabet :: V.Vector Character }

fromString :: String -> Alphabet
fromString = Alphabet. V.fromList . map c2w

-- | Alphabets.
--
-- Nucleotide IUPAC code (ordering according to list on Wikipedia).
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
--
-- Amino acid IUPAC type; normal amino acids in alphabetical order; then IUPAC
-- codes in alphabetical order.
--
-- @
-- Amino Acid Code:  Three letter Code:  Amino Acid:
-- ----------------  ------------------  -----------
-- A.................Ala.................Alanine
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
-- Y.................Tyr.................Tyrosine
-- B.................Asx.................Aspartic acid or Asparagine
-- X.................Xaa.................Any amino acid
-- Z.................Glx.................Glutamine or Glutamic acid
-- Additionally, I add:
-- -.................Gap.................No amino acid
-- @

alphabet :: Code -> Alphabet
alphabet DNA          = fromString "ACGT"
alphabet DNAIUPAC     = fromString "ACGTUWSMKRYBDHVNZ-"
alphabet Protein      = fromString "ACDEFGHIKLMNPQRSTVWY"
alphabet ProteinIUPAC = fromString "ACDEFGHIKLMNPQRSTVWYBXZ-"

-- | Alphabet optimized for lookups (i.e., "Is this character in the
-- alphabet?"). Order of characters is not preserved. 'Data.Set' is used because
-- it uses an ordered, tree-like structure with fast queries. When parsing
-- characters, they have to be checked for validity and so, the query speed is
-- very important when reading in large data files.
newtype AlphabetLookup = AlphabetLookup { fromAlphabetLookup :: S.Set Word8 }
  deriving (Show, Read, Eq, Ord)

-- Create an alphabet for lookups from 'Code'.
alphabetLookup' :: Code -> AlphabetLookup
alphabetLookup' = AlphabetLookup . S.fromList . V.toList . fromAlphabet . alphabet

-- | Create an alphabet for lookups from 'Code'; memoized.
alphabetLookup :: Code -> AlphabetLookup
alphabetLookup = Memo.enum alphabetLookup'

inAlphabet :: Code -> Character -> Bool
inAlphabet code char = toUpper char `S.member` fromAlphabetLookup (alphabetLookup code)

-- | Number of characters. Since for IUPAC codes, the cardinality is not
-- directly related to the number of characters in the alphabet, we have to set
-- it manually.
cardinality :: Code -> Int
cardinality DNA          = 4
cardinality DNAIUPAC     = 4
cardinality Protein      = 20
cardinality ProteinIUPAC = 20

-- | Convert integer index to 'Character'.
indexToCharacter :: Code -> Int -> Character
indexToCharacter code i = (fromAlphabet . alphabet $ code) V.! i

-- | Convert a character (Word8) to integer index in alphabet.
characterToIndex :: Code -> Word8 -> Int
characterToIndex DNA          char = fromEnum (fromWord char :: Nucleotide)
characterToIndex DNAIUPAC     char = fromEnum (fromWord char :: NucleotideIUPAC)
characterToIndex Protein      char = fromEnum (fromWord char :: AminoAcid)
characterToIndex ProteinIUPAC char = fromEnum (fromWord char :: AminoAcidIUPAC)
