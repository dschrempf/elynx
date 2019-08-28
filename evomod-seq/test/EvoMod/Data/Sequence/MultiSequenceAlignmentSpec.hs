{- |
Module      :  EvoMod.Data.Sequence.MultiSequenceAlignmentSpec
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 14:25:42 2018.

-}

module EvoMod.Data.Sequence.MultiSequenceAlignmentSpec
  (spec) where

import qualified Data.ByteString.Lazy.Char8                  as L
import qualified Data.Matrix.Unboxed                         as M
import           Test.Hspec

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.InputOutput
import           Files

ssData :: M.Matrix Character
ssData = M.fromLists $ map (reverse . map fromChar) [ "AAA", "GAA", "TAA" ]

ssMSA :: MultiSequenceAlignment
ssMSA = MultiSequenceAlignment (map L.pack ["SEQUENCE_1", "SEQUENCE_2", "SEQUENCE_3"]) DNAI ssData

spec :: Spec
spec =
  describe "subSample" $
  it "correctly sub sample an MSA" $ do
    msa <- either error id . fromSequenceList <$> parseFileWith (fasta DNAI) fastaNucleotideIUPACFN
    let ss = subSample [0,3,5] msa
    ss `shouldBe` ssMSA
