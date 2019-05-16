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

2. Sets of 'Character's such as nucleotides 'N.standard' or amino acids
   'A.standard'.

3. 'Alphabet's. The different 'Code's are collected in a specific data type. New
   codes have to be added manually in this module.

-}

module EvoMod.Data.Alphabet.Alphabet
  ( -- * Types
    Code (..)
  , codeNameVerbose
  , Alphabet (..)
  , alphabet
    -- * Queries
  , cardinality
  , indexToCharacter
  , characterToIndex
  , checkCharacter
    -- * IUPAC stuff
  , isStandard
  , isIUPAC
  , isGapOrUnknown
  , iupacToStandard
  ) where

import           Control.Exception.Base
import qualified Data.IntMap.Strict              as I
import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S

import qualified EvoMod.Data.Alphabet.AminoAcid  as A
import           EvoMod.Data.Alphabet.Character
import qualified EvoMod.Data.Alphabet.Nucleotide as N

-- | The used genetic code. Could include Protein_IUPAC, CountsFile for
-- population data and so on.
data Code = DNA | DNAX | DNAI | Protein | ProteinX | ProteinC | ProteinI
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Verbose version of code name.
codeNameVerbose :: Code -> String
codeNameVerbose DNA      = show DNA ++ " (nucleotides)"
codeNameVerbose DNAX     = show DNAX ++ " (nucleotides; extended; including gaps and unknowns)"
codeNameVerbose DNAI     = show DNAI ++ " (nucleotides; including IUPAC codes)"
codeNameVerbose Protein  = show Protein ++ " (amino acids)"
codeNameVerbose ProteinX = show ProteinX ++ " (amino acids; extended; including gaps and unknowns)"
codeNameVerbose ProteinC = show ProteinX ++ " (amino acids; extended plus '*' encoding stop codon)"
codeNameVerbose ProteinI = show ProteinI ++ " (amino acids; including IUPAC codes)"

-- | An alphabet is a vector of characters with a specific order.
newtype Alphabet = Alphabet { fromAlphabet :: [Character] }
  deriving (Show, Read, Eq, Ord)

toCharacters :: Alphabet -> [Character]
toCharacters = fromAlphabet

-- | Alphabets.
alphabet :: Code -> Alphabet
alphabet DNA      = Alphabet   N.standard
alphabet DNAX     = Alphabet $ N.standard ++ N.gap ++ N.unknown
alphabet DNAI     = Alphabet $ N.standard ++ N.iupac
alphabet Protein  = Alphabet   A.standard
alphabet ProteinX = Alphabet $ A.standard ++ A.gap ++ A.unknown
alphabet ProteinC = Alphabet $ A.standard ++ A.gap ++ A.unknown ++ [A.stop]
alphabet ProteinI = Alphabet $ A.standard ++ A.iupac

-- | Number of characters. For IUPAC codes, the cardinality is not the number of
-- characters in the standard alphabet.
cardinality :: Code -> Int
cardinality = length . fromAlphabet . alphabet

-- | Convert integer index to 'Character'.
indexToCharacter :: Code -> I.IntMap Character
indexToCharacter code = I.fromList $ zip [0..] (toCharacters . alphabet $ code)

-- | Convert a character (Word8) to integer index in alphabet.
characterToIndex :: Code -> M.Map Character Int
characterToIndex code = M.fromList $ zip (toCharacters . alphabet $ code) [0..]

-- | Check if character is part of code.
checkCharacter :: Code -> Character -> Bool
checkCharacter code char = char `elem` (fromAlphabet . alphabet) code

assertCharacter :: Code -> Character -> a -> a
assertCharacter code char = assert (checkCharacter code char)

nucleotides :: S.Set Character
nucleotides = S.fromList N.standard

aminoAcids :: S.Set Character
aminoAcids = S.fromList A.standard

-- Unsafe predicate.
isStandard' :: Code -> Character -> Bool
isStandard' DNA _         = True
isStandard' DNAX char     = char `S.member` nucleotides
isStandard' DNAI char     = char `S.member` nucleotides
isStandard' Protein _     = True
isStandard' ProteinX char = char `S.member` aminoAcids
isStandard' ProteinC char = char `S.member` aminoAcids
isStandard' ProteinI char = char `S.member` aminoAcids

-- | For a given code, is the character a standard character? A character is
-- considered standard, when it is not an extended IUPAC character.
isStandard :: Code -> Character -> Bool
isStandard code char = assertCharacter code char $ isStandard' code char

-- Unsafe predicate.
isIUPAC' :: Code -> Character -> Bool
isIUPAC' code char = not $ isStandard code char

-- | For a given code, is the character an extended IUPAC character? Assume that
-- the given character is part of the given alphabet. That is, if it is not
-- standard, it is assumed to be part of the IUPAC codes.
isIUPAC :: Code -> Character -> Bool
isIUPAC code char = assertCharacter code char $ isIUPAC' code char

-- Unsafe predicate.
isGapOrUnknown' :: Code -> Character -> Bool
isGapOrUnknown' code char = case code of
    DNA      -> False
    DNAX     -> char `S.member` ng
    DNAI     -> char `S.member` ng
    Protein  -> False
    ProteinX -> char `S.member` ag
    ProteinC -> char `S.member` ag
    ProteinI -> char `S.member` ag
  where
    ng = S.fromList $ N.gap ++ N.unknown
    ag = S.fromList $ A.gap ++ A.unknown

-- | For a given code, is the character unknown, or a gap? Assume that the given
-- character is part of the given alphabet.
isGapOrUnknown :: Code -> Character -> Bool
isGapOrUnknown code char = assertCharacter code char $ isGapOrUnknown' code char

-- Unsafe predicate.
iupacToStandard' :: Code -> Character -> [Character]
iupacToStandard' DNA      char = N.iupacToStandard M.! char
iupacToStandard' DNAX     char = N.iupacToStandard M.! char
iupacToStandard' DNAI     char = N.iupacToStandard M.! char
iupacToStandard' Protein  char = A.iupacToStandard M.! char
iupacToStandard' ProteinX char = A.iupacToStandard M.! char
iupacToStandard' ProteinC char = A.iupacToStandard M.! char
iupacToStandard' ProteinI char = A.iupacToStandard M.! char

-- | Convert from IUPAC character.
iupacToStandard :: Code -> Character -> [Character]
iupacToStandard code char = assertCharacter code char $ iupacToStandard' code char
