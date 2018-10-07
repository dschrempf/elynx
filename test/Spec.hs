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

import           Data.Either
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Test.Hspec
import           Text.Megaparsec

import           Base.MultiSequenceAlignment
import           Base.Nucleotide
import           Base.Sequence
import           EvolIO.Fasta

fastaNucleotideFN :: String
fastaNucleotideFN = "test/Data/Nucleotide.fasta"

fastaAminoAcidFN :: String
fastaAminoAcidFN = "test/Data/AminoAcid.fasta"

fastaErroneousFN :: String
fastaErroneousFN = "test/Data/Erroneous.fasta"

fastaDifferentLengthFN :: String
fastaDifferentLengthFN = "test/Data/NucleotideDifferentLength.fasta"

longestNamedSequenceInFile :: NamedSequence Nucleotide
longestNamedSequenceInFile =
  case parse parseSequence "" $ T.pack "ATTTAAAAAAACCCAAAACCCGGGCCCCGGGTTTTTTTA" of
    Left _ -> error "BAD. Basic sequence parser error."
    Right x -> NamedSequence "SEQUENCE_3" x

fastaDifferentLengthTrimmedFN :: String
fastaDifferentLengthTrimmedFN = "test/Data/NucleotideDifferentLengthTrimmed.fasta"

runParserOnFile :: Parsec e T.Text a -> String -> IO (Either (ParseError Char e) a)
runParserOnFile p f = parse p f <$> T.readFile f

main :: IO ()
main = hspec $ do
  describe "Base.Sequence.longest" $
    it "finds the longest sequence"$ do
    enss <- runParserOnFile fastaNucleotide fastaDifferentLengthFN
    enss `shouldSatisfy` isRight
    longestNamedSequence <$> enss `shouldBe` Right longestNamedSequenceInFile

  describe "Base.Sequence.filterLongerThan" $
    it "filters sequences that are longer than a specified length" $ do
    ens <- runParserOnFile fastaNucleotide fastaDifferentLengthFN
    ems <- runParserOnFile fastaNucleotide fastaDifferentLengthTrimmedFN
    filterLongerThanNamedSequence 10 <$> ens `shouldBe` ems

  describe "EvolIO.Fasta.fastaMSANucleotide" $ do
    it "parses a fasta file with nucleotide sequences with equal length" $ do
      emsa <- runParserOnFile fastaMSANucleotide fastaNucleotideFN
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
