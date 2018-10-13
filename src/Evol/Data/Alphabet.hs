{- |
Module      :  Evol.Data.Alphabet
Description :  Alphabets store hereditary information.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:57:08 2018.

-}

module Evol.Data.Alphabet
  ( Code (..)
  , Alphabet (..)
  , alphabet
  )
where

import qualified Data.Set             as S
import           Data.Word            (Word8)

import           Evol.Data.AminoAcid
import           Evol.Data.Character
import           Evol.Data.Nucleotide

-- | Protein_IUPAC, ...
data Code = DNA | DNA_IUPAC | Protein
  deriving (Read, Eq, Ord)

instance Show Code where
  show DNA       = "DNA (nucleotides)"
  show DNA_IUPAC = "DNA_IUPAC (nucleotides including IUPAC code)"
  show Protein   = "Protein (amino acids)"

-- | 'Data.Set' is used because it uses an ordered, tree-like structure with
-- fast queries.
newtype Alphabet = Alphabet { fromAlphabet :: S.Set Word8 }
  deriving (Show, Read, Eq, Ord)

toAlphabet :: Character a => [a] -> Alphabet
toAlphabet = Alphabet . S.fromList . map toWord

alphabet :: Code -> Alphabet
alphabet DNA       = toAlphabet [(minBound :: Nucleotide) .. ]
alphabet DNA_IUPAC = toAlphabet [(minBound :: NucleotideIUPAC) .. ]
alphabet Protein   = toAlphabet [(minBound :: AminoAcid) .. ]
