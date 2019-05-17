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
  ( Code (..)
  , alphabet
  , alphabetLookup
  ) where

import qualified Data.Set                        as S

import           EvoMod.Data.Alphabet.Character
import           EvoMod.Tools.Misc        (allValues)

-- | Available genetic codes.
data Code = DNA | DNAX | DNAI | Protein | ProteinX | ProteinI
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Complete alphabet comprising the code associated with the characters.
alphabet :: forall a . Character a => [a]
alphabet = allValues :: [a]

-- | Complete alphabet comprising the code associated with the characters.
-- Optimized for lookups.
alphabetLookup :: Character a => S.Set a
alphabetLookup = S.fromList alphabet
