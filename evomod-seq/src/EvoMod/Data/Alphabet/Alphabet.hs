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
    -- * IUPAC stuff
  , isStandard
  , isIUPAC
  , isGapOrUnknown
  , iupacToStandard
  ) where

import qualified Data.IntMap.Strict              as I
import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S

import qualified EvoMod.Data.Alphabet.AminoAcid  as A
import           EvoMod.Data.Alphabet.Character
import qualified EvoMod.Data.Alphabet.Nucleotide as N

-- | The used genetic code. Could include Protein_IUPAC, CountsFile for
-- population data and so on.
data Code = DNA | Protein
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Verbose version of code name.
codeNameVerbose :: Code -> String
codeNameVerbose DNA     = show DNA ++ " (nucleotides including IUPAC codes)"
codeNameVerbose Protein = show Protein ++ " (amino acids including IUPAC codes)"

-- | An alphabet is a vector of characters with a specific order.
newtype Alphabet = Alphabet { fromAlphabet :: [Character] }
  deriving (Show, Read, Eq, Ord)

toCharacters :: Alphabet -> [Character]
toCharacters = fromAlphabet

-- | Alphabets.
alphabet :: Code -> Alphabet
alphabet DNA     = Alphabet $ N.standard ++ N.iupac
alphabet Protein = Alphabet $ A.standard ++ A.iupac

-- | Number of characters. Since for IUPAC codes, the cardinality is not
-- directly related to the number of characters in the alphabet, we have to set
-- it manually.
cardinality :: Code -> Int
cardinality DNA     = 4
cardinality Protein = 20

-- | Convert integer index to 'Character'.
indexToCharacter :: Code -> I.IntMap Character
indexToCharacter code = I.fromList $ zip [0..] (toCharacters . alphabet $ code)

-- | Convert a character (Word8) to integer index in alphabet.
characterToIndex :: Code -> M.Map Character Int
characterToIndex code = M.fromList $ zip (toCharacters . alphabet $ code) [0..]

-- | For a given code, is the character a standard character? A character is
-- considered standard, when it is not an extended IUPAC character.
isStandard :: Code -> Character -> Bool
isStandard DNA          char = char `S.member` ns
  where ns = S.fromList N.standard
isStandard Protein      char = char `S.member` as
  where as = S.fromList A.standard

-- | For a given code, is the character an extended IUPAC character?
isIUPAC :: Code -> Character -> Bool
isIUPAC DNA          char = char `S.member` ni
  where ni = S.fromList N.iupac
isIUPAC Protein      char = char `S.member` ai
  where ai = S.fromList A.iupac

-- | For a given code, is the character unknown, or a gap?
isGapOrUnknown :: Code -> Character -> Bool
isGapOrUnknown DNA          char = char `S.member` ngu
  where ngu = S.fromList (N.gap ++ N.unknown)
isGapOrUnknown Protein      char = char `S.member` agu
  where agu = S.fromList (A.gap ++ A.unknown)

-- | Convert from IUPAC character.
iupacToStandard :: Code -> Character -> [Character]
iupacToStandard DNA          char = N.iupacToStandard M.! char
iupacToStandard Protein      char = A.iupacToStandard M.! char
