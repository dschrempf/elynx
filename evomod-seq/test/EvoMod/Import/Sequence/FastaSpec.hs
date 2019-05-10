{- |
Module      :  EvoMod.Import.Sequence.FastaSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 18 09:54:38 2019.

-}

module EvoMod.Import.Sequence.FastaSpec
  (spec) where

import           Data.Either
import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.InputOutput
import           Files
import           Test.Hspec

spec :: Spec
spec =
  describe "fastaFileMSA" $ do
    it "parses a fasta file with nucleotide sequences with equal length" $ do
      msa <- fromSequenceList <$> parseFileWith (fasta DNA) fastaNucleotideFN
      msaNSequences msa `shouldBe` 3
      msaLength msa `shouldBe` 40

    it "parses a fasta file with nucleotide IUPAC sequences with equal length" $ do
      msa <- fromSequenceList <$> parseFileWith (fasta DNA) fastaNucleotideIUPACFN
      msaNSequences msa `shouldBe` 3
      msaLength msa `shouldBe` 40

    it "should not parse erroneous files" $ do
      emsa <- runParserOnFile (fasta DNA) fastaErroneousFN
      emsa  `shouldSatisfy` isLeft

    it "parses a fasta file with amino acid sequences with equal length" $ do
      msa <- fromSequenceList <$> parseFileWith (fasta Protein) fastaAminoAcidFN
      msaNSequences msa `shouldBe` 2
      msaLength msa `shouldBe` 237

    it "should not parse erroneous files" $ do
      msa <- runParserOnFile (fasta Protein) fastaErroneousFN
      msa  `shouldSatisfy` isLeft

