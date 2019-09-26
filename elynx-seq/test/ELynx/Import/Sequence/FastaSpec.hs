{- |
Module      :  ELynx.Import.Sequence.FastaSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 18 09:54:38 2019.

-}

module ELynx.Import.Sequence.FastaSpec
  (spec) where

import           Data.Either
import           ELynx.Data.Alphabet.Alphabet
import qualified ELynx.Data.Sequence.MultiSequenceAlignment as M
import           ELynx.Import.Sequence.Fasta
import           ELynx.Tools.InputOutput
import           Files
import           Test.Hspec

spec :: Spec
spec =
  describe "fastaFileMSA" $ do
    it "parses a fasta file with nucleotide sequences with equal length" $ do
      msa <- either error id . M.fromSequences <$> parseFileWith (fasta DNA) fastaNucleotideFN
      M.nSequences msa `shouldBe` 3
      M.length msa `shouldBe` 40

    it "parses a fasta file with nucleotide IUPAC sequences with equal length" $ do
      msa <- either error id . M.fromSequences <$> parseFileWith (fasta DNAI) fastaNucleotideIUPACFN
      M.nSequences msa `shouldBe` 3
      M.length msa `shouldBe` 40

    it "should not parse erroneous files" $ do
      emsa <- runParserOnFile (fasta DNAI) fastaErroneousFN
      emsa  `shouldSatisfy` isLeft

    it "parses a fasta file with amino acid sequences with equal length" $ do
      msa <- either error id . M.fromSequences <$> parseFileWith (fasta Protein) fastaAminoAcidFN
      M.nSequences msa `shouldBe` 2
      M.length msa `shouldBe` 237

    it "should not parse erroneous files" $ do
      msa <- runParserOnFile (fasta ProteinI) fastaErroneousFN
      msa  `shouldSatisfy` isLeft

