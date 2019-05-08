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
import qualified Data.Matrix.Storable                        as M
import           Data.Word8
import           Test.Hspec

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.ByteString                     (c2w)
import           EvoMod.Tools.InputOutput
import           Files

ssData :: M.Matrix Word8
ssData = M.fromLists $ map (reverse . map c2w) [ "AAA", "GAA", "TAA" ]

ssMSA :: MultiSequenceAlignment
ssMSA = MultiSequenceAlignment (map L.pack ["SEQUENCE_1", "SEQUENCE_2", "SEQUENCE_3"]) DNAIUPAC ssData

spec :: Spec
spec =
  describe "subSample" $
  it "correctly sub sample an MSA" $ do
    msa <- fromSequenceList <$> parseFileWith (fasta DNAIUPAC) fastaNucleotideIUPACFN
    let ss = subSample [0,3,5] msa
    -- L.putStr $ summarizeMSA ss
    ss `shouldBe` ssMSA
