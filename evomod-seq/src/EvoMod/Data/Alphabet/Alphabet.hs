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
    alphabet
  , alphabetLookup
  ) where

import qualified Data.Set                         as S

import           EvoMod.Data.Alphabet.Character
import           EvoMod.Tools.Misc                (allValues)

-- | Complete alphabet comprising the code associated with the characters.
alphabet :: forall a . Character a => [a]
alphabet = allValues :: [a]

-- | Complete alphabet comprising the code associated with the characters.
-- Optimized for lookups.
alphabetLookup :: Character a => S.Set a
alphabetLookup = S.fromList alphabet
