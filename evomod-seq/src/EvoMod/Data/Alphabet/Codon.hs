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
  , UniversalCode (..)
  , universalCode
  ) where

import           Data.List
import qualified Data.Map                       as M

import           EvoMod.Data.Alphabet.Character

-- | Codons are triplets of characters.
newtype Codon = Codon (Character, Character, Character)
  deriving (Show, Read, Eq, Ord)

fromCharTriplet :: (Char, Char, Char) -> Codon
fromCharTriplet (c1, c2, c3) = Codon (fromChar c1, fromChar c2, fromChar c3)

-- | The different universal codes. See
-- https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c,
-- http://www.bioinformatics.org/sms2/genetic_code.html and
-- https://en.wikipedia.org/wiki/Genetic_code.
data UniversalCode = Standard | VertebrateMitochondrial
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- It is important that the map is lazy, because some keys have errors as values.
mapFromLists :: String -> String -> String -> String -> M.Map Codon Character
mapFromLists xs ys zs as = M.fromList $
  zipWith4 (\f s t a -> (fromCharTriplet (f, s, t), fromChar a)) xs ys zs as

-- Permutation of the triplets PLUS GAPS! I avoid 'Z' because I do not want to
-- translate DNAI.
base1, base2, base3 :: String
base1 = "TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG" ++ "-."
base2 = "TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG" ++ "-."
base3 = "TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG" ++ "-."

-- The actual codes.
standard :: String
standard =
  "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ++ "--"

vertebrateMitochondrial :: String
vertebrateMitochondrial =
  "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG" ++ "--"

-- | Map from 'Codon' to amino acid character.
universalCode :: UniversalCode -> M.Map Codon Character
universalCode Standard                = mapFromLists base1 base2 base3 standard
universalCode VertebrateMitochondrial = mapFromLists base1 base2 base3 vertebrateMitochondrial
