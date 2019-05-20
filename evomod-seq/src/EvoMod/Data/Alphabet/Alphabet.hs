{- |
Module      :  EvoMod.Data.Alphabet.Alphabet
Description :  Alphabets store hereditary information
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable

Portability :  portable

Creation date: Fri May 10 11:10:32 2019.

Hierarchy:

TODO. Create own character module.

TODO. Rename stuff. AlphabetName ? AlphabetSpec ? allCs ?

1. 'Character' type class.

2. Sets of 'Character's such as nucleotides or amino acids.

3. Alphabets. The different 'Code's are collected in a specific data type. New
   codes have to be added manually in this module.

-}

module EvoMod.Data.Alphabet.Alphabet
  (
    AlphabetName (..)
  , AlphabetSpec (..)
  , alphabetSpec
  , alphabetNameVerbose
  , isStd
  , isGap
  , isUnknown
  , isIUPAC
  , isMember
  ) where

import qualified Data.Set                       as S

import           EvoMod.Data.Alphabet.Character

data AlphabetName = DNA | DNAX | DNAI
                  | Protein | ProteinX | ProteinS | ProteinI
                  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Verbose alphabet name.
alphabetNameVerbose :: AlphabetName -> String
alphabetNameVerbose DNA      = "DNA (nucleotides)"
alphabetNameVerbose DNAX     = "DNAX (nucleotides; extended; including gaps and unknowns)"
alphabetNameVerbose DNAI     = "DNAI (nucleotides; including IUPAC codes)"
alphabetNameVerbose Protein  = "Protein (amino acids)"
alphabetNameVerbose ProteinX = "ProteinX (amino acids; extended; including gaps and unknowns)"
alphabetNameVerbose ProteinS = "ProteinS (amino acids; including gaps and translation stops)"
alphabetNameVerbose ProteinI = "ProteinI (amino acids; including IUPAC codes)"

data AlphabetSpec = AlphabetSpec { std     :: !(S.Set Character)
                                 , gap     :: !(S.Set Character)
                                 , unknown :: !(S.Set Character)
                                 , iupac   :: !(S.Set Character)
                                 , allCs   :: !(S.Set Character)
                                 , toStd   :: Character -> [Character] }

alphabetSpec :: AlphabetName -> AlphabetSpec
alphabetSpec DNA      = dna
alphabetSpec DNAX     = dnaX
alphabetSpec DNAI     = dnaI
alphabetSpec Protein  = protein
alphabetSpec ProteinX = proteinX
alphabetSpec ProteinS = proteinS
alphabetSpec ProteinI = proteinI

isWith :: (AlphabetSpec -> S.Set Character) -> AlphabetName -> Character -> Bool
isWith set alph char = char `S.member` set (alphabetSpec alph)

isStd :: AlphabetName -> Character -> Bool
isStd = isWith std

isGap :: AlphabetName -> Character -> Bool
isGap = isWith gap

isUnknown :: AlphabetName -> Character -> Bool
isUnknown = isWith unknown

isIUPAC :: AlphabetName -> Character -> Bool
isIUPAC = isWith iupac

isMember :: AlphabetName -> Character -> Bool
isMember = isWith allCs

fromChars :: String -> String -> String -> String -> (Char -> String) -> AlphabetSpec
fromChars st ga un iu to = AlphabetSpec st' ga' un' iu' al (fromString . to . toChar)
  where
    st' = S.fromList $ fromString st
    ga' = S.fromList $ fromString ga
    un' = S.fromList $ fromString un
    iu' = S.fromList $ fromString iu
    al  = S.unions [st', ga', un', iu']

dna :: AlphabetSpec
dna = fromChars "ACGT" [] [] [] toStdDNA

toStdDNA :: Char -> String
toStdDNA 'A' = "A"
toStdDNA 'C' = "C"
toStdDNA 'G' = "G"
toStdDNA 'T' = "T"
toStdDNA _   = error "tostdDNA: cannot convert to standard nucleotide."

dnaX :: AlphabetSpec
dnaX = fromChars "ACGT" "-." [] [] toStdDNAX

toStdDNAX :: Char -> String
toStdDNAX 'A' = "A"
toStdDNAX 'C' = "C"
toStdDNAX 'G' = "G"
toStdDNAX 'T' = "T"
toStdDNAX '-' = []
toStdDNAX '.' = []
toStdDNAX _   = error "toStdDNAX: cannot convert to standard nucleotide."

dnaI :: AlphabetSpec
dnaI = fromChars "ACGT" "-." "N" "UWSMKRYBDHV" toStdDNAI

toStdDNAI :: Char -> String
toStdDNAI 'A' = "A"
toStdDNAI 'C' = "C"
toStdDNAI 'G' = "G"
toStdDNAI 'T' = "T"
toStdDNAI 'U' = "T"
toStdDNAI 'W' = "AT"
toStdDNAI 'S' = "GC"
toStdDNAI 'M' = "AC"
toStdDNAI 'K' = "GT"
toStdDNAI 'R' = "AG"
toStdDNAI 'Y' = "CT"
toStdDNAI 'B' = "CGT"
toStdDNAI 'D' = "AGT"
toStdDNAI 'H' = "ACT"
toStdDNAI 'V' = "ACG"
toStdDNAI 'N' = "ACGT"
toStdDNAI '-' = []
toStdDNAI '.' = []
toStdDNAI _   = error "toStdDNAI: cannot convert to standard nucleotide."

protein :: AlphabetSpec
protein = fromChars "ACDEFGHIKLMNPQRSTVWY" [] [] [] toStdP

toStdP :: Char -> String
toStdP 'A' = "A"
toStdP 'C' = "C"
toStdP 'D' = "D"
toStdP 'E' = "E"
toStdP 'F' = "F"
toStdP 'G' = "G"
toStdP 'H' = "H"
toStdP 'I' = "I"
toStdP 'K' = "K"
toStdP 'L' = "L"
toStdP 'M' = "M"
toStdP 'N' = "N"
toStdP 'P' = "P"
toStdP 'Q' = "Q"
toStdP 'R' = "R"
toStdP 'S' = "S"
toStdP 'T' = "T"
toStdP 'V' = "V"
toStdP 'W' = "W"
toStdP 'Y' = "Y"
toStdP _   = error "toStdP: cannot convert to standard amino acid."

proteinX :: AlphabetSpec
proteinX = fromChars "ACDEFGHIKLMNPQRSTVWY" "-." [] [] toStdPX

toStdPX :: Char -> String
toStdPX 'A' = "A"
toStdPX 'C' = "C"
toStdPX 'D' = "D"
toStdPX 'E' = "E"
toStdPX 'F' = "F"
toStdPX 'G' = "G"
toStdPX 'H' = "H"
toStdPX 'I' = "I"
toStdPX 'K' = "K"
toStdPX 'L' = "L"
toStdPX 'M' = "M"
toStdPX 'N' = "N"
toStdPX 'P' = "P"
toStdPX 'Q' = "Q"
toStdPX 'R' = "R"
toStdPX 'S' = "S"
toStdPX 'T' = "T"
toStdPX 'V' = "V"
toStdPX 'W' = "W"
toStdPX 'Y' = "Y"
toStdPX '-' = ""
toStdPX '.' = ""
toStdPX _   = error "toStdPX: cannot convert to standard amino acid."

proteinS :: AlphabetSpec
proteinS = fromChars "ACDEFGHIKLMNPQRSTVWY" "-." [] "*" toStdPS

toStdPS :: Char -> String
toStdPS 'A' = "A"
toStdPS 'C' = "C"
toStdPS 'D' = "D"
toStdPS 'E' = "E"
toStdPS 'F' = "F"
toStdPS 'G' = "G"
toStdPS 'H' = "H"
toStdPS 'I' = "I"
toStdPS 'K' = "K"
toStdPS 'L' = "L"
toStdPS 'M' = "M"
toStdPS 'N' = "N"
toStdPS 'P' = "P"
toStdPS 'Q' = "Q"
toStdPS 'R' = "R"
toStdPS 'S' = "S"
toStdPS 'T' = "T"
toStdPS 'V' = "V"
toStdPS 'W' = "W"
toStdPS 'Y' = "Y"
toStdPS '-' = ""
toStdPS '.' = ""
toStdPS '*' = ""
toStdPS _   = error "toStdPX: cannot convert to standard amino acid."

proteinI :: AlphabetSpec
proteinI = fromChars "ACDEFGHIKLMNPQRSTVWY" "-." "X" "*JBZ" toStdPI

toStdPI :: Char -> String
toStdPI 'A' = "A"
toStdPI 'C' = "C"
toStdPI 'D' = "D"
toStdPI 'E' = "E"
toStdPI 'F' = "F"
toStdPI 'G' = "G"
toStdPI 'H' = "H"
toStdPI 'I' = "I"
toStdPI 'K' = "K"
toStdPI 'L' = "L"
toStdPI 'M' = "M"
toStdPI 'N' = "N"
toStdPI 'P' = "P"
toStdPI 'Q' = "Q"
toStdPI 'R' = "R"
toStdPI 'S' = "S"
toStdPI 'T' = "T"
toStdPI 'V' = "V"
toStdPI 'W' = "W"
toStdPI 'Y' = "Y"
toStdPI '-' = ""
toStdPI '.' = ""
toStdPI '*' = ""
toStdPI 'J' = "LI"
toStdPI 'B' = "DN"
toStdPI 'Z' = "EQ"
toStdPI 'X' = "ACDEFGHIKLMNPQRSTVWY"
toStdPI _   = error "toStdPX: cannot convert to standard amino acid."
