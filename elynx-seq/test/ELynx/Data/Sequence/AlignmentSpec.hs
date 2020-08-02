-- |
-- Module      :  ELynx.Data.Sequence.AlignmentSpec
-- Copyright   :  (c) Dominik Schrempf 2018
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 14:25:42 2018.
module ELynx.Data.Sequence.AlignmentSpec
  ( spec,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Matrix.Unboxed as M
import ELynx.Data.Alphabet.Alphabet
import ELynx.Data.Alphabet.Character
import ELynx.Data.Sequence.Alignment
import ELynx.Import.Sequence.Fasta
import Test.Hspec

fastaNucleotideIUPACFN :: FilePath
fastaNucleotideIUPACFN = "data/NucleotideIUPAC.fasta"

ssData :: M.Matrix Character
ssData = M.fromLists $ map (reverse . map fromChar) ["AAA", "GAA", "TAA"]

ssA :: Alignment
ssA =
  Alignment
    (map BL.pack ["SEQUENCE_1", "SEQUENCE_2", "SEQUENCE_3"])
    (replicate 3 BL.empty)
    DNAI
    ssData

spec :: Spec
spec = describe "subSample" $
  it "correctly sub sample an Alignment" $ do
    a <-
      either error id
        . fromSequences
        <$> parseFileWith (fasta DNAI) fastaNucleotideIUPACFN
    let ss = subSample [0, 3, 5] a
    ss `shouldBe` ssA
