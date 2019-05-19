{- |
Module      :  EvoMod.Data.Character.Codon
Description :  Codons are triplets of nucleotides
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu May 16 07:58:50 2019.

The different universal codes.
- https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c
- http://www.bioinformatics.org/sms2/genetic_code.html
- https://en.wikipedia.org/wiki/Genetic_code

-}

module EvoMod.Data.Character.Codon
  ( Codon (Codon)
  , UniversalCode (..)
  , universalCode
  , universalCodeX
  ) where

import           Data.List
import qualified Data.Map                         as M

import           EvoMod.Data.Character.AminoAcidS
import qualified EvoMod.Data.Character.Nucleotide  as N
import qualified EvoMod.Data.Character.NucleotideX as NX

-- | Codons are triplets of characters.
newtype Codon a = Codon (a, a, a)
  deriving (Show, Read, Eq, Ord)

-- | Universal codes.
data UniversalCode = Standard | VertebrateMitochondrial
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- It is important that the map is lazy, because some keys have errors as values.
mapFromLists :: Ord a => [a] -> [a] -> [a]
             -> [b] -> M.Map (Codon a) b
mapFromLists xs ys zs as = M.fromList $
  zipWith4 (\f s t a -> (Codon (f, s, t), a)) xs ys zs as

nucs :: Enum a => [a]
nucs = map toEnum [3,1,0,2]     -- Order T, C, A , G.

-- Permutation of the triplets PLUS GAPS! I avoid 'Z' because I do not want to
-- translate DNAI.
base1, base2, base3 :: Enum a => [a]
base1 = [n | n <- nucs
           , _ <- [0..3 :: Int]
           , _ <- [0..3 :: Int]]
-- base1 = "TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG" ++ "-."
base2 = [n | _ <- [0..3 :: Int]
           , n <- nucs
           , _ <- [0..3 :: Int]]
-- base2 = "TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG" ++ "-."
base3 = [n | _ <- [0..3 :: Int]
           , _ <- [0..3 :: Int]
           , n <- nucs]
-- base3 = "TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG" ++ "-."

-- The actual codes.
standard :: [AminoAcidS]
standard = [ F, F, L, L, S, S, S, S, Y, Y, Stop, Stop, C, C, Stop, W, L, L, L,
             L, P, P, P, P, H, H, Q, Q, R, R, R, R, I, I, I, M, T, T, T, T, N,
             N, K, K, S, S, R, R, V, V, V, V, A, A, A, A, D, D, E, E, G, G, G,
             G]
-- "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ++ "--"

vertebrateMitochondrial :: [AminoAcidS]
vertebrateMitochondrial = [F, F, L, L, S, S, S, S, Y, Y, Stop, Stop, C, C, W, W,
                           L, L, L, L, P, P, P, P, H, H, Q, Q, R, R, R, R, I, I,
                           M, M, T, T, T, T, N, N, K, K, S, S, Stop, Stop, V, V,
                           V, V, A, A, A, A, D, D, E, E, G, G, G, G]
-- "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG" ++ "--"

-- | Map from 'Codon' to amino acid character.
universalCode :: UniversalCode -> M.Map (Codon N.Nucleotide) AminoAcidS
universalCode Standard                = mapFromLists base1 base2 base3 standard
universalCode VertebrateMitochondrial = mapFromLists base1 base2 base3 vertebrateMitochondrial

-- | Map from 'Codon' to amino acid character.
universalCodeX :: UniversalCode -> M.Map (Codon NX.NucleotideX) AminoAcidS
universalCodeX Standard                = mapFromLists
                                         (base1 ++ [NX.Gap])
                                         (base2 ++ [NX.Gap])
                                         (base3 ++ [NX.Gap])
                                         (standard ++ [Gap])
universalCodeX VertebrateMitochondrial = mapFromLists
                                         (base1 ++ [NX.Gap])
                                         (base2 ++ [NX.Gap])
                                         (base3 ++ [NX.Gap])
                                         (vertebrateMitochondrial ++ [Gap])
