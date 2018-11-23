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

module EvoMod.Data.Alphabet
  ( Code (..)
  , codeNameVerbose
  , Alphabet (..)
  , alphabet
  )
where

import qualified Data.Set             as S
import           Data.Word            (Word8)

import           EvoMod.Data.AminoAcid
import           EvoMod.Data.Character
import           EvoMod.Data.Nucleotide

-- | The used genetic code. Could include Protein_IUPAC, CountsFile for
-- population data and so on.
data Code = DNA | DNA_IUPAC | Protein
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

codeNameVerbose :: Code -> String
codeNameVerbose DNA       = "DNA (nucleotides)"
codeNameVerbose DNA_IUPAC = "DNA_IUPAC (nucleotides including IUPAC code)"
codeNameVerbose Protein   = "Protein (amino acids)"

-- | 'Data.Set' is used because it uses an ordered, tree-like structure with
-- fast queries. When parsing characters, they have to be checked for validity
-- and so, the query speed is very important when reading in large data files.
newtype Alphabet = Alphabet { fromAlphabet :: S.Set Word8 }
  deriving (Show, Read, Eq, Ord)

-- | Since 'Character's are required to be enumerated and bounded, we can
-- calculate the corresponding alphabet.
toAlphabet :: Character a => [a] -> Alphabet
toAlphabet = Alphabet . S.fromList . map toWord

-- | New codes have to be added manually here. I tried to use type classes, so
-- that each character has to supply an alphabet, but then the language
-- extension TypeApplications has to be added. Like this, new codes have to be
-- added manually, but the type handling is cleaner.
alphabet :: Code -> Alphabet
alphabet DNA       = toAlphabet [(minBound :: Nucleotide) .. ]
alphabet DNA_IUPAC = toAlphabet [(minBound :: NucleotideIUPAC) .. ]
alphabet Protein   = toAlphabet [(minBound :: AminoAcid) .. ]
