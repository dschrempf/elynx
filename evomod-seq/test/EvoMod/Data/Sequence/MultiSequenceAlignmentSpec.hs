{-# LANGUAGE TypeApplications #-}

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

import           EvoMod.Data.Alphabet.NucleotideI
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.InputOutput
import           Files

ssData :: M.Matrix NucleotideI
ssData = M.fromLists $ map (reverse . map fromChar) [ "AAA", "GAA", "TAA" ]

ssMSA :: MultiSequenceAlignment NucleotideI
ssMSA = MultiSequenceAlignment (map L.pack ["SEQUENCE_1", "SEQUENCE_2", "SEQUENCE_3"]) ssData

spec :: Spec
spec =
  describe "subSample" $
  it "correctly sub sample an MSA" $ do
    msa <- fromSequenceList <$> parseFileWith (fasta @NucleotideI) fastaNucleotideIUPACFN
    let ss = subSample [0,3,5] msa
    -- L.putStr $ summarizeMSA ss
    ss `shouldBe` ssMSA
