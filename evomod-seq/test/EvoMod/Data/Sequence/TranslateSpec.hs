{-# LANGUAGE TypeApplications #-}

{- |
Module      :  EvoMod.Data.Sequence.TranslateSpec
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 14:25:42 2018.

-}

module EvoMod.Data.Sequence.TranslateSpec
  (spec) where

import           Test.Hspec

import           EvoMod.Data.Alphabet.NucleotideX
import           EvoMod.Data.Alphabet.AminoAcidS
import           EvoMod.Data.Alphabet.Codon
import           EvoMod.Data.Sequence.Translate
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.InputOutput
import           Files

spec :: Spec
spec =
  describe "translateDNAX" $
    it "correctly translates a test sequence" $ do
    ss  <- parseFileWith (fasta @NucleotideX) fastaTranslateDNAFN
    ss' <- parseFileWith (fasta @AminoAcidS) fastaTranslateProteinFN
    map (translateDNAX VertebrateMitochondrial 0) ss `shouldBe` ss'
