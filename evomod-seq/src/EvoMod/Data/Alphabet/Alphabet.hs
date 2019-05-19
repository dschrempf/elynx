{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

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

TODO.

1. 'Character' type class.

2. Sets of 'Character's such as nucleotides or amino acids.

3. Alphabets. The different 'Code's are collected in a specific data type. New
   codes have to be added manually in this module.

-}

module EvoMod.Data.Alphabet.Alphabet
  (
    Character
  , Alphabet
  , AlphabetSpec
  , alphabet
  ) where

import qualified Data.Set                     as S
import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import           EvoMod.Tools.ByteString      (c2w, w2c)

data Alphabet = DNA | DNAX | DNAI
              | Protein | ProteinX | ProteinS | ProteinI
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype Character = Character Word8
  deriving (Read, Show, Eq, Ord, Bounded)

toChar :: Character -> Char
toChar (Character w) = w2c w

fromChar :: Char -> Character
fromChar = Character . c2w

toString :: [Character] -> String
toString = map toChar

fromString :: String -> [Character]
fromString = map fromChar

derivingUnbox "Character"
    [t| Character -> Word8 |]
    [| \(Character w) -> w |]
    [| Character |]

data AlphabetSpec = AlphabetSpec { standard   :: S.Set Character
                                 , gap        :: S.Set Character
                                 , iupac      :: S.Set Character
                                 , alphabet   :: S.Set Character
                                 , toStandard :: Character -> [Character] }

fromChars :: String -> String -> String -> (Char -> String) -> AlphabetSpec
fromChars st ga iu toSt = AlphabetSpec st' ga' iu' al (fromString . toSt . toChar)
  where
    st' = S.fromList $ fromString st
    ga' = S.fromList $ fromString ga
    iu' = S.fromList $ fromString iu
    al  = S.unions [st', ga', iu']

dna :: AlphabetSpec
dna = fromChars "ACGT" "" "" toStandardDNA

toStandardDNA :: Char -> String
toStandardDNA 'A' = "A"
toStandardDNA 'C' = "C"
toStandardDNA 'G' = "G"
toStandardDNA 'T' = "T"
toStandardDNA _   = error "dnaToStandard: cannot convert to standard nucleotide."

dnaX :: AlphabetSpec
dnaX = fromChars "ACGT" "-." "" toStandardDNAX

toStandardDNAX :: Char -> String
toStandardDNAX 'A' = "A"
toStandardDNAX 'C' = "C"
toStandardDNAX 'G' = "G"
toStandardDNAX 'T' = "T"
toStandardDNAX '-' = ""
toStandardDNAX '.' = ""
toStandardDNAX _   = error "dnaToStandard: cannot convert to standard nucleotide."

dnaI :: AlphabetSpec
dnaI = fromChars "ACGT" "-." "UWSMKRYBDHV" toStandardDNAI

toStandardDNAI :: Char -> String
toStandardDNAI 'A' = "A"
toStandardDNAI 'C' = "C"
toStandardDNAI 'G' = "G"
toStandardDNAI 'T' = "T"
toStandardDNAI 'U' = "T"
toStandardDNAI 'W' = "AT"
toStandardDNAI 'S' = "GC"
toStandardDNAI 'M' = "AC"
toStandardDNAI 'K' = "GT"
toStandardDNAI 'R' = "AG"
toStandardDNAI 'Y' = "CT"
toStandardDNAI 'B' = "CGT"
toStandardDNAI 'D' = "AGT"
toStandardDNAI 'H' = "ACT"
toStandardDNAI 'V' = "ACG"
toStandardDNAI 'N' = "ACGT"
toStandardDNAI '-' = ""
toStandardDNAI '.' = ""

codeSpec :: Alphabet -> AlphabetSpec
codeSpec = undefined

-- | Verbose code name.
codeNameVerbose :: Alphabet -> String
codeNameVerbose DNA      = "DNA (nucleotides)"
codeNameVerbose DNAX     = "DNAX (nucleotides; extended; including gaps and unknowns)"
codeNameVerbose DNAI     = "DNAI (nucleotides; including IUPAC codes)"
codeNameVerbose Protein  = "Protein (amino acids)"
codeNameVerbose ProteinX = "ProteinX (amino acids; extended; including gaps and unknowns)"
codeNameVerbose ProteinS = "ProteinS (amino acids; including gaps and translation stops)"
codeNameVerbose ProteinI = "ProteinI (amino acids; including IUPAC codes)"
