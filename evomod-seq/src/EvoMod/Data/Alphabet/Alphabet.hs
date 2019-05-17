{-# LANGUAGE ScopedTypeVariables #-}

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
  , alphabet
  , alphabetLookup
    -- * Queries
  , cardinality
  ) where

import qualified Data.Set                        as S

import           EvoMod.Data.Alphabet.Character
import           EvoMod.Tools.Misc        (allValues)

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

-- | The complete alphabet comprising the code associated with the characters.
alphabet :: forall a . Character a => [a]
alphabet = allValues :: [a]

-- | The complete alphabet comprising the code associated with the characters.
-- Optimized for lookups.
alphabetLookup :: Character a => S.Set a
alphabetLookup = S.fromList alphabet

-- | Number of characters. For IUPAC codes, the cardinality is not the number of
-- characters in the standard alphabet.
cardinality :: [a] -> Int
cardinality = length
