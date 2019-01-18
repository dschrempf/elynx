{- |
Module      :  EvoMod.Data.Sequence.SequenceSpec
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 14:25:42 2018.

-}

module EvoMod.Data.Sequence.SequenceSpec
  (spec) where

import qualified Data.ByteString.Lazy.Char8    as B
import           Test.Hspec

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools
import           Files

longestSequenceInFileBS :: B.ByteString
longestSequenceInFileBS = B.unlines $ map B.pack [ ">SEQUENCE_3"
                                                 , "ATTTAAAAAAACCCAAAACCCGGGCCCCGGGTTTTTTTA" ]

longestSequenceInFile :: Sequence
longestSequenceInFile = parseByteStringWith (fastaSequence $ alphabet DNA) longestSequenceInFileBS

spec :: Spec
spec =
  describe "longest" $
    it "finds the longest sequence"$ do
    ss <- parseFileWith (fastaFile DNA) fastaDifferentLengthFN
    longest ss `shouldBe` longestSequenceInFile


