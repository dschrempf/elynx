{- |
Module      :  EvoMod.Data.Alphabet
Description :  Alphabets store hereditary information.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:57:08 2018.

Hierarchy:

1. 'Character' type class.

2. 'Character' instances such as 'Nucleotide's or 'AminoAcid's.

3. The character types form 'Alphabet's. The different 'Code's are collected in
   a specific data type. New codes have to be added manually in this module.

-}

module EvoMod.Data.Alphabet.Alphabet
  ( Code (..)
  , codeNameVerbose
  , Alphabet (..)
  , alphabet
  , AlphabetLookup (..)
  , alphabetLookup
  , inAlphabet
  , cardinality
  , cardinalityFromCode
  , indexToCharacter
  , characterToIndex
  , indicesToCharacters
  , fromIUPAC
  )
where

import qualified Data.Set                        as S
import qualified Data.Vector.Storable            as V
import           Data.Word8                      (Word8, toUpper)

import           EvoMod.Data.Alphabet.AminoAcid
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Alphabet.Nucleotide
import           EvoMod.Tools.Misc               (allValues)

-- | The used genetic code. Could include Protein_IUPAC, CountsFile for
-- population data and so on.
data Code = DNA | DNA_IUPAC | Protein | ProteinIUPAC
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Verbose version of code name.
codeNameVerbose :: Code -> String
codeNameVerbose DNA          = show DNA ++ " (nucleotides)"
codeNameVerbose DNA_IUPAC    = show DNA_IUPAC ++ " (nucleotides including IUPAC codes)"
codeNameVerbose Protein      = show Protein ++ " (amino acids)"
codeNameVerbose ProteinIUPAC = show ProteinIUPAC ++ " (amino acids including IUPAC codes)"

-- | An alphabet is a vector of characters with a specific order.
newtype Alphabet = Alphabet { fromAlphabet :: V.Vector Word8 }
  deriving (Show, Read, Eq, Ord)

toAlphabet :: Character a => [a] -> Alphabet
toAlphabet = Alphabet . V.fromList . map toWord

-- | Create alphabet from 'Code'. New codes have to be added manually here. I
-- tried to use type classes, so that each character has to supply an alphabet,
-- but then the language extension TypeApplications has to be added. Like this,
-- new codes have to be added manually, but the type handling is cleaner.
alphabet :: Code -> Alphabet
alphabet DNA          = toAlphabet (allValues :: [Nucleotide])
alphabet DNA_IUPAC    = toAlphabet (allValues :: [NucleotideIUPAC])
alphabet Protein      = toAlphabet (allValues :: [AminoAcid])
alphabet ProteinIUPAC = toAlphabet (allValues :: [AminoAcidIUPAC])

-- | Alphabet optimized for lookups (i.e., "Is this character in the
-- alphabet?"). Order of characters is not preserved. 'Data.Set' is used because
-- it uses an ordered, tree-like structure with fast queries. When parsing
-- characters, they have to be checked for validity and so, the query speed is
-- very important when reading in large data files.
newtype AlphabetLookup = AlphabetLookup { fromAlphabetLookup :: S.Set Word8 }
  deriving (Show, Read, Eq, Ord)

-- | Create an alphabet for lookups from 'Code'.
alphabetLookup :: Code -> AlphabetLookup
alphabetLookup = AlphabetLookup . S.fromList . V.toList . fromAlphabet . alphabet

-- | For a given code, check if character is in alphabet.
inAlphabet :: Code -> Word8 -> Bool
inAlphabet code char = toUpper char `S.member` fromAlphabetLookup (alphabetLookup code)

-- | The cardinality of an alphabet is the number of entries.
cardinality :: Alphabet -> Int
cardinality = V.length . fromAlphabet

-- | Number of characters
cardinalityFromCode :: Code -> Int
cardinalityFromCode = cardinality . alphabet

-- | Convert integer index to 'Character'.
indexToCharacter :: Code -> Int -> Word8
indexToCharacter code i = (fromAlphabet . alphabet $ code) V.! i

-- | Convert a character (Word8) to integer index in alphabet.
characterToIndex :: Code -> Word8 -> Int
characterToIndex DNA          char = fromEnum (fromWord char :: Nucleotide)
characterToIndex DNA_IUPAC    char = fromEnum (fromWord char :: NucleotideIUPAC)
characterToIndex Protein      char = fromEnum (fromWord char :: AminoAcid)
characterToIndex ProteinIUPAC char = fromEnum (fromWord char :: AminoAcidIUPAC)

-- | Convert a set of integer indices to 'Character's.
indicesToCharacters :: Code -> [Int] -> [Word8]
indicesToCharacters c = map (indexToCharacter c)

-- | Convert from IUPAC.
fromIUPAC :: Code -> Word8 -> (Code, [Word8])
fromIUPAC DNA          char = (DNA,     [char])
fromIUPAC DNA_IUPAC    char = (DNA,     map toWord $ fromIUPACNucleotide
                                (fromWord char :: NucleotideIUPAC))
fromIUPAC Protein      char = (Protein, [char])
fromIUPAC ProteinIUPAC char = (Protein, map toWord $ fromIUPACAminoAcid
                                (fromWord char :: AminoAcidIUPAC))
