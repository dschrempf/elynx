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

import qualified Data.ByteString.Lazy.Char8       as B (pack)
import           Data.Either
import           Test.Hspec
import           Text.Megaparsec

import           Evol.Data.Alphabet
import           Evol.Data.MultiSequenceAlignment
import           Evol.Data.Nucleotide
import           Evol.Data.Sequence
import           Evol.IO.Fasta
import           Evol.Tools                       (runParserOnFile)

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

longestSequenceInFile :: Sequence String Nucleotide
longestSequenceInFile =
  case parse (some parseChar) "" $ B.pack "ATTTAAAAAAACCCAAAACCCGGGCCCCGGGTTTTTTTA" of
    Left _  -> error "BAD. Basic sequence parser error."
    Right x -> Sequence "SEQUENCE_3" x

fastaDifferentLengthTrimmedFN :: String
fastaDifferentLengthTrimmedFN = "test/Data/NucleotideDifferentLengthTrimmed.fasta"

main :: IO ()
main = hspec $ do
  describe "Base.Sequence.longest" $
    it "finds the longest sequence"$ do
    enss <- runParserOnFile fastaNucleotide fastaDifferentLengthFN
    enss `shouldSatisfy` isRight
    longest <$> enss `shouldBe` Right longestSequenceInFile

  describe "Base.Sequence.filterLongerThan" $
    it "filters sequences that are longer than a specified length" $ do
    ens <- runParserOnFile fastaNucleotide fastaDifferentLengthFN
    ems <- runParserOnFile fastaNucleotide fastaDifferentLengthTrimmedFN
    filterLongerThan 10 <$> ens `shouldBe` ems

  describe "EvolIO.Fasta.fastaMSANucleotide" $ do
    it "parses a fasta file with nucleotide sequences with equal length" $ do
      emsa <- runParserOnFile fastaMSANucleotide fastaNucleotideFN
      emsa  `shouldSatisfy` isRight
      msaNSequences <$> emsa `shouldBe` Right (3 ::Int)
      msaLength <$> emsa `shouldBe` Right 40

    it "parses a fasta file with nucleotide IUPAC sequences with equal length" $ do
      emsa <- runParserOnFile fastaMSANucleotideIUPAC fastaNucleotideIUPACFN
      emsa  `shouldSatisfy` isRight
      msaNSequences <$> emsa `shouldBe` Right (3 ::Int)
      msaLength <$> emsa `shouldBe` Right 40

    it "should not parse erroneous files" $ do
      emsa <- runParserOnFile fastaNucleotide fastaErroneousFN
      emsa  `shouldSatisfy` isLeft

  describe "EvolIO.Fasta.fastaMSAAminoAcid" $ do
    it "parses a fasta file with amino acid sequences with equal length" $ do
      emsa <- runParserOnFile fastaMSAAminoAcid fastaAminoAcidFN
      emsa  `shouldSatisfy` isRight
      msaNSequences <$> emsa `shouldBe` Right (2 ::Int)
      msaLength <$> emsa `shouldBe` Right 237

    it "should not parse erroneous files" $ do
      emsa <- runParserOnFile fastaAminoAcid fastaErroneousFN
      emsa  `shouldSatisfy` isLeft
