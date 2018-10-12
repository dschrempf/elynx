{- |
Module      :  Spec
Description :  Test EvolIO.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 14:25:42 2018.

-}


module Main where

import qualified Data.ByteString.Lazy.Char8       as B
import           Data.Either
import           Test.Hspec

import           Evol.Data.AminoAcid
import           Evol.Data.MultiSequenceAlignment
import           Evol.Data.Nucleotide
import           Evol.Data.Sequence
import           Evol.IO.Fasta
import           Evol.Tools

fastaNucleotideFN :: String
fastaNucleotideFN = "test/Data/Nucleotide.fasta"

fastaNucleotideIUPACFN :: String
fastaNucleotideIUPACFN = "test/Data/NucleotideIUPAC.fasta"

fastaAminoAcidFN :: String
fastaAminoAcidFN = "test/Data/AminoAcid.fasta"

fastaErroneousFN :: String
fastaErroneousFN = "test/Data/Erroneous.fasta"

fastaDifferentLengthFN :: String
fastaDifferentLengthFN = "test/Data/NucleotideDifferentLength.fasta"

longestSequenceInFileBS :: B.ByteString
longestSequenceInFileBS = B.unlines $ map B.pack [ ">SEQUENCE_3"
                                                 , "ATTTAAAAAAACCCAAAACCCGGGCCCCGGGTTTTTTTA" ]

longestSequenceInFile :: Sequence
longestSequenceInFile = parseByteStringWith (fastaSequence nucleotides) longestSequenceInFileBS

fastaDifferentLengthTrimmedFN :: String
fastaDifferentLengthTrimmedFN = "test/Data/NucleotideDifferentLengthTrimmed.fasta"

main :: IO ()
main = hspec $ do
  describe "Base.Sequence.longest" $
    it "finds the longest sequence"$ do
    ss <- parseFileWith (fastaFile nucleotides) fastaDifferentLengthFN
    longest ss `shouldBe` longestSequenceInFile

  describe "Base.Sequence.filterLongerThan" $
    it "filters sequences that are longer than a specified length" $ do
    ss  <- parseFileWith (fastaFile nucleotides) fastaDifferentLengthFN
    ss' <- parseFileWith (fastaFile nucleotides) fastaDifferentLengthTrimmedFN
    filterLongerThan 10 ss `shouldBe` ss'

  describe "EvolIO.Fasta.fastaMSANucleotide" $ do
    it "parses a fasta file with nucleotide sequences with equal length" $ do
      msa <- parseFileWith (fastaFileMSA nucleotides) fastaNucleotideFN
      msaNSequences msa `shouldBe` 3
      msaLength msa `shouldBe` 40

    it "parses a fasta file with nucleotide IUPAC sequences with equal length" $ do
      msa <- parseFileWith (fastaFileMSA nucleotidesIUPAC) fastaNucleotideIUPACFN
      msaNSequences msa `shouldBe` 3
      msaLength msa `shouldBe` 40

    it "should not parse erroneous files" $ do
      emsa <- runParserOnFile (fastaFile nucleotides) fastaErroneousFN
      emsa  `shouldSatisfy` isLeft

  describe "EvolIO.Fasta.fastaMSAAminoAcid" $ do
    it "parses a fasta file with amino acid sequences with equal length" $ do
      msa <- parseFileWith (fastaFileMSA aminoAcids) fastaAminoAcidFN
      msaNSequences msa `shouldBe` 2
      msaLength msa `shouldBe` 237

    it "should not parse erroneous files" $ do
      msa <- runParserOnFile (fastaFile aminoAcids) fastaErroneousFN
      msa  `shouldSatisfy` isLeft

  describe "EvolIO.Fasta.sequencesToFasta" $
    it "should create a fasta bytestring that, when parsed again, is the original sequence" $ do
      ss <- parseFileWith (fastaFile nucleotidesIUPAC) fastaNucleotideIUPACFN
      let f   = sequencesToFasta ss
          ss' = parseByteStringWith (fastaFile nucleotidesIUPAC) f
      ss `shouldBe` ss'

