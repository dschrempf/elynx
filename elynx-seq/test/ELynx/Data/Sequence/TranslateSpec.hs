-- |
-- Module      :  ELynx.Data.Sequence.TranslateSpec
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 14:25:42 2018.
module ELynx.Data.Sequence.TranslateSpec
  ( spec,
  )
where

import ELynx.Data.Alphabet.Alphabet
import ELynx.Data.Character.Codon
import ELynx.Data.Sequence.Translate
import ELynx.Import.Sequence.Fasta
import ELynx.Tools
import Test.Hspec

fastaTranslateDNAFN :: FilePath
fastaTranslateDNAFN = "data/TranslateMitochondrialVertebrateDNA.fasta"

fastaTranslateProteinFN :: FilePath
fastaTranslateProteinFN = "data/TranslateMitochondrialVertebrateProtein.fasta"

spec :: Spec
spec =
  describe "translateDNAX" $
    it "correctly translates a test sequence" $ do
      ss <- parseFileWith (fasta DNAX) fastaTranslateDNAFN
      ss' <- parseFileWith (fasta ProteinS) fastaTranslateProteinFN
      map (translateSeq VertebrateMitochondrial 0) ss `shouldBe` ss'
