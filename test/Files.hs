{- |
Module      :  Files
Description :  File names of test data.
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
  , componentsFilePhyloBayes
  ) where

dataDir :: String
dataDir = "data/"

fastaNucleotideFN :: String
fastaNucleotideFN = dataDir ++ "Nucleotide.fasta"

fastaNucleotideIUPACFN :: String
fastaNucleotideIUPACFN = dataDir ++ "NucleotideIUPAC.fasta"

fastaAminoAcidFN :: String
fastaAminoAcidFN = dataDir ++ "AminoAcid.fasta"

fastaDifferentLengthFN :: String
fastaDifferentLengthFN = dataDir ++ "NucleotideDifferentLength.fasta"

fastaDifferentLengthTrimmedFN :: String
fastaDifferentLengthTrimmedFN = dataDir ++ "NucleotideDifferentLengthTrimmed.fasta"

fastaErroneousFN :: String
fastaErroneousFN = dataDir ++ "Erroneous.fasta"

componentsFilePhyloBayes :: FilePath
componentsFilePhyloBayes = dataDir ++ "EDMDistsPhyloBayes.txt"

