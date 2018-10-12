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
import qualified Evol.Data.AminoAcid  as AA
import qualified Evol.Data.Nucleotide as DNA

data AlphabetName = DNA | DNA_IUPAC | AA
  deriving (Read, Eq, Ord)

instance Show AlphabetName where
  show DNA       = "Nucleotides (DNA)"
  show DNA_IUPAC = "Nucleotides including IUPAC code (DNA_IUPAC)"
  show AA        = "Amino acids (AA)"

alphabet :: AlphabetName -> Alphabet
alphabet DNA       = DNA.alphabet
alphabet DNA_IUPAC = DNA.alphabetIUPAC
alphabet AA        = AA.alphabet
