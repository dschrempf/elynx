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

import           Data.Attoparsec.Text        (parseOnly)
import           Data.Either
import qualified Data.Text.IO                as T
import           Test.Hspec

import           Base.MultiSequenceAlignment
import           EvolIO.Fasta

fastaNucleotideFN :: String
fastaNucleotideFN = "test/Data/Nucleotide.fasta"

fastaAminoAcidFN :: String
fastaAminoAcidFN = "test/Data/AminoAcid.fasta"

fastaErroneousFN :: String
fastaErroneousFN = "test/Data/Erroneous.fasta"

main :: IO ()
main = hspec $ do
  describe "EvolIO.Fasta.fastaNucleotide" $ do
    it "parses a fasta file containing nucleotides" $ do
      input <- T.readFile fastaNucleotideFN
      let emsa = parseOnly fastaNucleotide input
      emsa  `shouldSatisfy` isRight
      nSequences <$> emsa `shouldBe` Right (3 ::Int)
      lengthMSA <$> emsa `shouldBe` Right (Just 40)

    it "should not parse erroneous files" $ do
      input <- T.readFile fastaErroneousFN
      let emsa = parseOnly fastaNucleotide input
      emsa  `shouldSatisfy` isLeft
      print emsa

  describe "EvolIO.Fasta.fastaAminoAcid" $ do
    it "parses a fasta file containing amino acids" $ do
      input <- T.readFile fastaAminoAcidFN
      let emsa = parseOnly fastaAminoAcid input
      emsa  `shouldSatisfy` isRight
      nSequences <$> emsa `shouldBe` Right (2 ::Int)
      lengthMSA <$> emsa `shouldBe` Right (Just 237)

    it "should not parse erroneous files" $ do
      input <- T.readFile fastaErroneousFN
      let emsa = parseOnly fastaAminoAcid input
      emsa  `shouldSatisfy` isLeft
