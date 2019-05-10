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

1. 'Character' type.

2. Sets of 'Character's such as 'Nucleotide's or 'AminoAcid's.

3. 'Alphabet's. The different 'Code's are collected in a specific data type. New
   codes have to be added manually in this module.

-}

module EvoMod.Data.Alphabet.Alphabet
  (
    -- * Data types
    Code (..)
  , codeNameVerbose
  , Alphabet
  , alphabet
  , AlphabetLookup
  , alphabetLookup
    -- * Lookup
  , inAlphabet
  , cardinality
  , indexToCharacterMap
  , characterToIndexMap
    -- * Classes
  , isStandard
  , isExtendedIUPAC
  , isGapOrUnknown
  ) where

import qualified Data.IntMap.Strict              as IntMap
import qualified Data.Map.Strict                 as Map
import qualified Data.MemoCombinators            as Memo
import qualified Data.Set                        as Set
import qualified Data.Vector.Unboxed             as Vec
import           Data.Word8

import           EvoMod.Data.Alphabet.AminoAcid
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Alphabet.Nucleotide

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
newtype Alphabet = Alphabet { fromAlphabet :: Vec.Vector Character }

fromCharacters :: [Character] -> Alphabet
fromCharacters = Alphabet. Vec.fromList

toCharacters :: Alphabet -> [Character]
toCharacters = Vec.toList . fromAlphabet

-- | Alphabets.
alphabet :: Code -> Alphabet
alphabet DNA          = fromCharacters nucleotides
alphabet DNAIUPAC     = fromCharacters nucleotidesIUPAC
alphabet Protein      = fromCharacters aminoAcids
alphabet ProteinIUPAC = fromCharacters aminoAcidsIUPAC

-- | Alphabet optimized for lookups (i.e., "Is this character in the
-- alphabet?"). Order of characters is not preserved. 'Data.Set' is used because
-- it uses an ordered, tree-like structure with fast queries. When parsing
-- characters, they have to be checked for validity and so, the query speed is
-- very important when reading in large data files.
newtype AlphabetLookup = AlphabetLookup { fromAlphabetLookup :: Set.Set Word8 }
  deriving (Show, Read, Eq, Ord)

-- Create an alphabet for lookups from 'Code'.
alphabetLookup' :: Code -> AlphabetLookup
alphabetLookup' = AlphabetLookup . Set.fromList . Vec.toList . fromAlphabet . alphabet

-- | Create an alphabet for lookups from 'Code'; memoized.
alphabetLookup :: Code -> AlphabetLookup
alphabetLookup = Memo.enum alphabetLookup'

inAlphabet :: Code -> Character -> Bool
inAlphabet code char = toUpper char `Set.member` fromAlphabetLookup (alphabetLookup code)

-- | Number of characters. Since for IUPAC codes, the cardinality is not
-- directly related to the number of characters in the alphabet, we have to set
-- it manually.
cardinality :: Code -> Int
cardinality DNA          = 4
cardinality DNAIUPAC     = 4
cardinality Protein      = 20
cardinality ProteinIUPAC = 20

-- | Convert integer index to 'Character'.
indexToCharacterMap :: Code -> IntMap.IntMap Character
indexToCharacterMap code = IntMap.fromList $ zip [0..] (toCharacters . alphabet $ code)

-- | Convert a character (Word8) to integer index in alphabet.
characterToIndexMap :: Code -> Map.Map Character Int
characterToIndexMap code = Map.fromList $ zip (toCharacters . alphabet $ code) [0..]

-- | Get normal code from IUPAC code.
fromIUPAC :: Code -> Code
fromIUPAC DNA          = DNA
fromIUPAC DNAIUPAC     = DNA
fromIUPAC Protein      = Protein
fromIUPAC ProteinIUPAC = Protein

-- | Get IUPAC code from normal code.
toIUPAC :: Code -> Code
toIUPAC DNA          = DNAIUPAC
toIUPAC DNAIUPAC     = DNAIUPAC
toIUPAC Protein      = ProteinIUPAC
toIUPAC ProteinIUPAC = ProteinIUPAC

-- XXX: Probably assume that character is in alphabet? Then it would be much faster.
isStandard :: Code -> Character -> Bool
isStandard code = inAlphabet (fromIUPAC code)

isExtendedIUPAC :: Code -> Character -> Bool
isExtendedIUPAC code char = not (isStandard code char) && inAlphabet (toIUPAC code) char

isGapOrUnknown :: Code -> Character -> Bool
isGapOrUnknown DNA          = isGapOrUnknownNucleotide
isGapOrUnknown DNAIUPAC     = isGapOrUnknownNucleotide
isGapOrUnknown Protein      = isGapOrUnknownAminoAcid
isGapOrUnknown ProteinIUPAC = isGapOrUnknownAminoAcid

-- -- | Convert from IUPAC character.
-- charFromIUPAC :: Code -> Character -> [Character]
-- charFromIUPAC DNA          char = [char]
-- charFromIUPAC DNAIUPAC     char = map toWord $ fromIUPACNucleotide (fromWord char :: NucleotideIUPAC)
-- charFromIUPAC Protein      char = [char]
-- charFromIUPAC ProteinIUPAC char = map toWord $ fromIUPACAminoAcid (fromWord char :: AminoAcidIUPAC)

-- {- |
-- Module      :  EvoMod.Data.Alphabet
-- Description :  Alphabets store hereditary information
-- Copyright   :  (c) Dominik Schrempf 2018
-- License     :  GPL-3

-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable

-- Creation date: Thu Oct  4 18:57:08 2018.

-- Hierarchy:

-- 1. 'Character' type class.

-- 2. 'Character' instances such as 'Nucleotide's or 'AminoAcid's.

-- 3. The character types form 'Alphabet's. The different 'Code's are collected in
--    a specific data type. New codes have to be added manually in this module.

-- -}

-- module EvoMod.Data.Alphabet.Alphabet
--   ( Code (..)
--   , codeNameVerbose
--   , Alphabet (..)
--   , alphabet
--   , AlphabetLookup (..)
--   , alphabetLookup
--   , inAlphabet
--   , cardinality
--   , indexToCharacter
--   , characterToIndex
--   , indicesToCharacters
--   , fromIUPAC
--   , charFromIUPAC
--   , isStandardW
--   , areStandardW
--   , isGapOrUnknownW
--   , areGapOrUnknownW
--   ) where

-- import qualified Data.MemoCombinators            as Memo
-- import qualified Data.Set                        as S
-- import qualified Data.Vector.Storable            as V
-- import           Data.Word8                      (Word8, toUpper)

-- import           EvoMod.Data.Alphabet.AminoAcid
-- import           EvoMod.Data.Alphabet.Character
-- import           EvoMod.Data.Alphabet.Nucleotide
-- import           EvoMod.Tools.Misc               (allValues)

-- -- | The used genetic code. Could include Protein_IUPAC, CountsFile for
-- -- population data and so on.
-- data Code = DNA | DNAIUPAC | Protein | ProteinIUPAC
--   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- -- | Verbose version of code name.
-- codeNameVerbose :: Code -> String
-- codeNameVerbose DNA          = show DNA ++ " (nucleotides)"
-- codeNameVerbose DNAIUPAC     = show DNAIUPAC ++ " (nucleotides including IUPAC codes)"
-- codeNameVerbose Protein      = show Protein ++ " (amino acids)"
-- codeNameVerbose ProteinIUPAC = show ProteinIUPAC ++ " (amino acids including IUPAC codes)"

-- -- | An alphabet is a vector of characters with a specific order.
-- newtype Alphabet = Alphabet { fromAlphabet :: V.Vector Word8 }
--   deriving (Show, Read, Eq, Ord)

-- toAlphabet :: Character a => [a] -> Alphabet
-- toAlphabet = Alphabet . V.fromList . map toWord

-- -- | Create alphabet from 'Code'. New codes have to be added manually here. I
-- -- tried to use type classes, so that each character has to supply an alphabet,
-- -- but then the language extension TypeApplications has to be added. Like this,
-- -- new codes have to be added manually, but the type handling is cleaner.
-- alphabet :: Code -> Alphabet
-- alphabet DNA          = toAlphabet (allValues :: [Nucleotide])
-- alphabet DNAIUPAC     = toAlphabet (allValues :: [NucleotideIUPAC])
-- alphabet Protein      = toAlphabet (allValues :: [AminoAcid])
-- alphabet ProteinIUPAC = toAlphabet (allValues :: [AminoAcidIUPAC])

-- -- | Alphabet optimized for lookups (i.e., "Is this character in the
-- -- alphabet?"). Order of characters is not preserved. 'Data.Set' is used because
-- -- it uses an ordered, tree-like structure with fast queries. When parsing
-- -- characters, they have to be checked for validity and so, the query speed is
-- -- very important when reading in large data files.
-- newtype AlphabetLookup = AlphabetLookup { fromAlphabetLookup :: S.Set Word8 }
--   deriving (Show, Read, Eq, Ord)

-- -- Create an alphabet for lookups from 'Code'.
-- alphabetLookup' :: Code -> AlphabetLookup
-- alphabetLookup' = AlphabetLookup . S.fromList . V.toList . fromAlphabet . alphabet

-- -- | Create an alphabet for lookups from 'Code'; memoized.
-- alphabetLookup :: Code -> AlphabetLookup
-- alphabetLookup = Memo.enum alphabetLookup'

-- -- | For a given code, check if character is in alphabet. Although sets and
-- -- memoization is used, a direct check with 'S.member' is faster!
-- inAlphabet :: Word8 -> Code -> Bool
-- inAlphabet char code = toUpper char `S.member` fromAlphabetLookup (alphabetLookup code)

-- -- | Number of characters. Since for IUPAC codes, the cardinality is not
-- -- directly related to the number of characters in the alphabet, we have to set
-- -- it manually.
-- cardinality :: Code -> Int
-- cardinality DNA          = 4
-- cardinality DNAIUPAC     = 4
-- cardinality Protein      = 20
-- cardinality ProteinIUPAC = 20

-- -- | Convert integer index to 'Character'.
-- indexToCharacter :: Code -> Int -> Word8
-- indexToCharacter code i = (fromAlphabet . alphabet $ code) V.! i

-- -- | Convert a character (Word8) to integer index in alphabet.
-- characterToIndex :: Code -> Word8 -> Int
-- characterToIndex DNA          char = fromEnum (fromWord char :: Nucleotide)
-- characterToIndex DNAIUPAC     char = fromEnum (fromWord char :: NucleotideIUPAC)
-- characterToIndex Protein      char = fromEnum (fromWord char :: AminoAcid)
-- characterToIndex ProteinIUPAC char = fromEnum (fromWord char :: AminoAcidIUPAC)

-- -- | Convert a set of integer indices to 'Character's.
-- indicesToCharacters :: Code -> [Int] -> [Word8]
-- indicesToCharacters c = map (indexToCharacter c)

-- -- | Get normal code from IUPAC code.
-- fromIUPAC :: Code -> Code
-- fromIUPAC DNA          = DNA
-- fromIUPAC DNAIUPAC     = DNA
-- fromIUPAC Protein      = Protein
-- fromIUPAC ProteinIUPAC = Protein

-- -- -- | Check if code contains extnded IUPAC chracter set.
-- -- isIUPACCode :: Code -> Bool
-- -- isIUPACCode DNA          = False
-- -- isIUPACCode DNAIUPAC     = True
-- -- isIUPACCode Protein      = False
-- -- isIUPACCode ProteinIUPAC = True

-- -- | Convert from IUPAC character.
-- charFromIUPAC :: Code -> Word8 -> [Word8]
-- charFromIUPAC DNA          char = [char]
-- charFromIUPAC DNAIUPAC     char = map toWord $ fromIUPACNucleotide (fromWord char :: NucleotideIUPAC)
-- charFromIUPAC Protein      char = [char]
-- charFromIUPAC ProteinIUPAC char = map toWord $ fromIUPACAminoAcid (fromWord char :: AminoAcidIUPAC)

-- -- | Is the character c a standard character, then @isStandard c@ is @True@, or
-- -- an extended IUPAC character, then @isStandard c@ is @False@. This will not be
-- -- too fast for many comparisons because the code has to be converted to the
-- -- non-IUPAC equivalent for each comparison; use 'areStandardW'.
-- isStandardW :: Word8 -> Code -> Bool
-- isStandardW char code = char `inAlphabet` fromIUPAC code

-- -- | See 'isStandardW' but for many characters.
-- areStandardW :: V.Vector Word8 -> Code -> V.Vector Bool
-- areStandardW chars code = V.map (`inAlphabet` codeNonIUPAC) chars
--   where codeNonIUPAC = fromIUPAC code

-- -- | Check if character is a gap (usually @-@), or an unknown character (usually
-- -- @N@).
-- isGapOrUnknownW :: Word8 -> Code -> Bool
-- isGapOrUnknownW _ DNA             = False
-- isGapOrUnknownW char DNAIUPAC     = isGapOrUnknown (fromWord char :: NucleotideIUPAC)
-- isGapOrUnknownW _ Protein         = False
-- isGapOrUnknownW char ProteinIUPAC = isGapOrUnknown (fromWord char :: AminoAcidIUPAC)

-- -- | Check if characters are gaps (usually @-@), or an unknown characters
-- -- (usually @N@).
-- areGapOrUnknownW :: V.Vector Word8 -> Code -> V.Vector Bool
-- areGapOrUnknownW chars code = V.map (`isGapOrUnknownW` code) chars
