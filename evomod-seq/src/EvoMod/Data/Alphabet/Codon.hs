{- |
Module      :  EvoMod.Data.Alphabet.Codon
Description :  Codons are triplets of nucleotides
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu May 16 07:58:50 2019.

-}

module EvoMod.Data.Alphabet.Codon
  ( Codon (Codon)
  , universalCode
  ) where

import qualified Data.Map                       as M

import           EvoMod.Data.Alphabet.Character

newtype Codon = Codon (Character, Character, Character)
  deriving (Show, Read, Eq, Ord)

fromCharTriplet :: (Char, Char, Char) -> Codon
fromCharTriplet (c1, c2, c3) = Codon (fromChar c1, fromChar c2, fromChar c3)

-- | See https://en.wikipedia.org/wiki/Genetic_code.
universalCode :: M.Map Codon Character
universalCode = M.fromList $ map (\(k, v) -> (fromCharTriplet k, fromChar v))
  [
    -- First column (second base is T|U).
    (('T', 'T', 'T'), 'F')
  , (('T', 'T', 'C'), 'F')
  , (('T', 'T', 'A'), 'L')
  , (('T', 'T', 'G'), 'L')

  , (('C', 'T', 'T'), 'L')
  , (('C', 'T', 'C'), 'L')
  , (('C', 'T', 'A'), 'L')
  , (('C', 'T', 'G'), 'L')

  , (('A', 'T', 'T'), 'I')
  , (('A', 'T', 'C'), 'I')
  , (('A', 'T', 'A'), 'I')
  , (('A', 'T', 'G'), 'M')

  , (('G', 'T', 'T'), 'V')
  , (('G', 'T', 'C'), 'V')
  , (('G', 'T', 'A'), 'V')
  , (('G', 'T', 'G'), 'V')

    -- Second column (second base is C).
  , (('T', 'C', 'T'), 'S')
  , (('T', 'C', 'C'), 'S')
  , (('T', 'C', 'A'), 'S')
  , (('T', 'C', 'G'), 'S')

  , (('C', 'C', 'T'), 'P')
  , (('C', 'C', 'C'), 'P')
  , (('C', 'C', 'A'), 'P')
  , (('C', 'C', 'G'), 'P')

  , (('A', 'C', 'T'), 'T')
  , (('A', 'C', 'C'), 'T')
  , (('A', 'C', 'A'), 'T')
  , (('A', 'C', 'G'), 'T')

  , (('G', 'C', 'T'), 'A')
  , (('G', 'C', 'C'), 'A')
  , (('G', 'C', 'A'), 'A')
  , (('G', 'C', 'G'), 'A')

    -- Third column (second base is A).
  , (('T', 'A', 'T'), 'Y')
  , (('T', 'A', 'C'), 'Y')
  , (('T', 'A', 'A'), error "universalCode: cannot translate stop codon Ochre.")
  , (('T', 'A', 'G'), error "universalCode: cannot translate stop codon Amber.")

  , (('C', 'A', 'T'), 'H')
  , (('C', 'A', 'C'), 'H')
  , (('C', 'A', 'A'), 'Q')
  , (('C', 'A', 'G'), 'Q')

  , (('A', 'A', 'T'), 'N')
  , (('A', 'A', 'C'), 'N')
  , (('A', 'A', 'A'), 'K')
  , (('A', 'A', 'G'), 'K')

  , (('G', 'A', 'T'), 'D')
  , (('G', 'A', 'C'), 'D')
  , (('G', 'A', 'A'), 'E')
  , (('G', 'A', 'G'), 'E')

    -- Fourth column (second base is G).
  , (('T', 'G', 'T'), 'C')
  , (('T', 'G', 'C'), 'C')
  , (('T', 'G', 'A'), error "universalCode: cannot translate stop codon Opal.")
  , (('T', 'G', 'G'), 'W')

  , (('C', 'G', 'T'), 'R')
  , (('C', 'G', 'C'), 'R')
  , (('C', 'G', 'A'), 'R')
  , (('C', 'G', 'G'), 'R')

  , (('A', 'G', 'T'), 'S')
  , (('A', 'G', 'C'), 'S')
  , (('A', 'G', 'A'), 'R')
  , (('A', 'G', 'G'), 'R')

  , (('G', 'G', 'T'), 'G')
  , (('G', 'G', 'C'), 'G')
  , (('G', 'G', 'A'), 'G')
  , (('G', 'G', 'G'), 'G')

  -- TODO: See below, ideally we want to go back to a more strict alphabet. For
  -- now, just also translate gap characters (., and -). No 'Z', because we
  -- don't want to accidentally translate DNAI.
  , (('.', '.', '.'), '-')
  , (('-', '-', '-'), '-')
                           ]

-- -- Ideally, we want something like this.
-- newtype Codon = Codon (Nucleotide, Nucleotide, Nucleotide)
--   deriving (Show, Read, Eq, Ord, Bounded)

-- fromCharacterTriplet :: (Character, Character, Character) -> Codon
-- fromCharacterTriplet (c1, c2, c3) = Codon (fromCharacter c1, fromCharacter c2, fromCharacter c3)

-- -- | See https://en.wikipedia.org/wiki/Genetic_code.
-- universalCode :: M.Map Codon A.AminoAcid
-- universalCode = M.fromList
--   [
--     -- First column (second base is T|U).
--     (Codon (T, T, T), A.F)
--   , (Codon (T, T, C), A.F)
--   , (Codon (T, T, A), A.L)
--   , (Codon (T, T, G), A.L)

--   , (Codon (C, T, T), A.L)
--   , (Codon (C, T, C), A.L)
--   , (Codon (C, T, A), A.L)
--   , (Codon (C, T, G), A.L)

--   , (Codon (A, T, T), A.I)
--   , (Codon (A, T, C), A.I)
--   , (Codon (A, T, A), A.I)
--   , (Codon (A, T, G), A.M)

--   , (Codon (G, T, T), A.V)
--   , (Codon (G, T, C), A.V)
--   , (Codon (G, T, A), A.V)
--   , (Codon (G, T, G), A.V)

--     -- Second column (second base is C).
--   , (Codon (T, C, T), A.S)
--   , (Codon (T, C, C), A.S)
--   , (Codon (T, C, A), A.S)
--   , (Codon (T, C, G), A.S)

--   , (Codon (C, C, T), A.P)
--   , (Codon (C, C, C), A.P)
--   , (Codon (C, C, A), A.P)
--   , (Codon (C, C, G), A.P)

--   , (Codon (A, C, T), A.T)
--   , (Codon (A, C, C), A.T)
--   , (Codon (A, C, A), A.T)
--   , (Codon (A, C, G), A.T)

--   , (Codon (G, C, T), A.A)
--   , (Codon (G, C, C), A.A)
--   , (Codon (G, C, A), A.A)
--   , (Codon (G, C, G), A.A)

--     -- Third column (second base is A).
--   , (Codon (T, A, T), A.Y)
--   , (Codon (T, A, C), A.Y)
--   , (Codon (T, A, A), error "universalCode: cannot translate stop codon Ochre.")
--   , (Codon (T, A, G), error "universalCode: cannot translate stop codon Amber.")

--   , (Codon (C, A, T), A.H)
--   , (Codon (C, A, C), A.H)
--   , (Codon (C, A, A), A.Q)
--   , (Codon (C, A, G), A.Q)

--   , (Codon (A, A, T), A.N)
--   , (Codon (A, A, C), A.N)
--   , (Codon (A, A, A), A.K)
--   , (Codon (A, A, G), A.K)

--   , (Codon (G, A, T), A.D)
--   , (Codon (G, A, C), A.D)
--   , (Codon (G, A, A), A.E)
--   , (Codon (G, A, G), A.E)

--     -- Fourth column (second base is G).
--   , (Codon (T, G, T), A.C)
--   , (Codon (T, G, C), A.C)
--   , (Codon (T, G, A), error "universalCode: cannot translate stop codon Opal.")
--   , (Codon (T, G, G), A.W)

--   , (Codon (C, G, T), A.R)
--   , (Codon (C, G, C), A.R)
--   , (Codon (C, G, A), A.R)
--   , (Codon (C, G, G), A.R)

--   , (Codon (A, G, T), A.S)
--   , (Codon (A, G, C), A.S)
--   , (Codon (A, G, A), A.R)
--   , (Codon (A, G, G), A.R)

--   , (Codon (G, G, T), A.G)
--   , (Codon (G, G, C), A.G)
--   , (Codon (G, G, A), A.G)
--   , (Codon (G, G, G), A.G)
--                            ]
