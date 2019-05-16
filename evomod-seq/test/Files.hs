{- |
Module      :  Files
Description :  File names of test data
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 18 10:03:02 2019.

-}

module Files
  ( fastaNucleotideFN
  , fastaNucleotideIUPACFN
  , fastaAminoAcidFN
  , fastaDifferentLengthFN
  , fastaDifferentLengthTrimmedFN
  , fastaErroneousFN
  , componentsFilePhylobayes
  , fastaTranslateDNAFN
  , fastaTranslateProteinFN
  ) where

dataDir :: FilePath
dataDir = "data/"

fastaNucleotideFN :: FilePath
fastaNucleotideFN = dataDir ++ "Nucleotide.fasta"

fastaNucleotideIUPACFN :: FilePath
fastaNucleotideIUPACFN = dataDir ++ "NucleotideIUPAC.fasta"

fastaAminoAcidFN :: FilePath
fastaAminoAcidFN = dataDir ++ "AminoAcid.fasta"

fastaDifferentLengthFN :: FilePath
fastaDifferentLengthFN = dataDir ++ "NucleotideDifferentLength.fasta"

fastaDifferentLengthTrimmedFN :: FilePath
fastaDifferentLengthTrimmedFN = dataDir ++ "NucleotideDifferentLengthTrimmed.fasta"

fastaErroneousFN :: FilePath
fastaErroneousFN = dataDir ++ "Erroneous.fasta"

componentsFilePhylobayes :: FilePath
componentsFilePhylobayes = dataDir ++ "EDMDistsPhylobayes.txt"

fastaTranslateDNAFN :: FilePath
fastaTranslateDNAFN = dataDir ++ "TranslateMitochondrialVertebrateDNA.fasta"

fastaTranslateProteinFN :: FilePath
fastaTranslateProteinFN = dataDir ++ "TranslateMitochondrialVertebrateProtein.fasta"
