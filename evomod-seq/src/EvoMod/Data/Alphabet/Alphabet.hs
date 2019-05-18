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

TODO.

1. 'Character' type class.

2. Sets of 'Character's such as nucleotides or amino acids.

3. Alphabets. The different 'Code's are collected in a specific data type. New
   codes have to be added manually in this module.

-}

module EvoMod.Data.Alphabet.Alphabet
  (
    C
  , Alphabet
  , AlphabetSpec
  , alphabet
  ) where

import qualified Data.Set   as S
import           Data.Word8

newtype C = C Word8

data AlphabetSpec = AlphabetSpec { standard   :: S.Set C
                                 , gap        :: S.Set C
                                 , iupac      :: S.Set C
                                 , toStandard :: C -> [C] }

data Alphabet = DNA | DNAX | DNAI
              | AminoAcid | AminoAcidX | AminoAcidS | AminoAcidI

alphabet :: Alphabet -> AlphabetSpec
alphabet = undefined

-- TODO.
-- -- | Available genetic codes.
-- data Code = DNA | DNAX | DNAI | Protein | ProteinX | ProteinS | ProteinI
--   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- -- | Verbose code name.
-- codeNameVerbose :: Code -> String
-- codeNameVerbose DNA      = "DNA (nucleotides)"
-- codeNameVerbose DNAX     = "DNAX (nucleotides; extended; including gaps and unknowns)"
-- codeNameVerbose DNAI     = "DNAI (nucleotides; including IUPAC codes)"
-- codeNameVerbose Protein  = "Protein (amino acids)"
-- codeNameVerbose ProteinX = "ProteinX (amino acids; extended; including gaps and unknowns)"
-- codeNameVerbose ProteinS = "ProteinS (amino acids; including gaps and translation stops)"
-- codeNameVerbose ProteinI = "ProteinI (amino acids; including IUPAC codes)"
