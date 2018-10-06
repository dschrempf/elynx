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
import           EvolIO.Fasta

fastaNucleotideFN :: String
fastaNucleotideFN = "test/Data/Nucleotide.fasta"

fastaAminoAcidFN :: String
fastaAminoAcidFN = "test/Data/AminoAcid.fasta"

fastaErroneousFN :: String
fastaErroneousFN = "test/Data/Erroneous.fasta"


runParserOnFile :: Parsec e T.Text a -> String -> IO (Either (ParseError Char e) a)
runParserOnFile p f = parse p f <$> T.readFile f

main :: IO ()
main = hspec $ do
  describe "EvolIO.Fasta.fastaNucleotide" $ do
    it "parses a fasta file containing nucleotides" $ do
      emsa <- runParserOnFile fastaMSANucleotide fastaNucleotideFN
      emsa  `shouldSatisfy` isRight
      nSequences <$> emsa `shouldBe` Right (3 ::Int)
      nSites <$> emsa `shouldBe` Right 40

    it "should not parse erroneous files" $ do
      emsa <- runParserOnFile fastaNucleotide fastaErroneousFN
      emsa  `shouldSatisfy` isLeft

  describe "EvolIO.Fasta.fastaAminoAcid" $ do
    it "parses a fasta file containing amino acids" $ do
      emsa <- runParserOnFile fastaMSAAminoAcid fastaAminoAcidFN
      emsa  `shouldSatisfy` isRight
      nSequences <$> emsa `shouldBe` Right (2 ::Int)
      nSites <$> emsa `shouldBe` Right 237

    it "should not parse erroneous files" $ do
      emsa <- runParserOnFile fastaAminoAcid fastaErroneousFN
      emsa  `shouldSatisfy` isLeft
