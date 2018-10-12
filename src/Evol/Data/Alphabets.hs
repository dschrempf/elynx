{- |
Module      :  Alphabets
Description :  Available alphabets.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct 12 12:23:22 2018.

-}


module Evol.Data.Alphabets
  ( AlphabetName (..)
  , alphabet
  ) where

import           Evol.Data.Alphabet
import           Evol.Data.AminoAcid
import           Evol.Data.Nucleotide

data AlphabetName = DNA | DNA_IUPAC | Protein
  deriving (Read, Eq, Ord)

instance Show AlphabetName where
  show DNA       = "DNA (nucleotides)"
  show DNA_IUPAC = "DNA_IUPAC (nucleotides including IUPAC code)"
  show Protein   = "Protein (amino acids)"

alphabet :: AlphabetName -> Alphabet
alphabet DNA       = nucleotides
alphabet DNA_IUPAC = nucleotidesIUPAC
alphabet Protein   = aminoAcids
