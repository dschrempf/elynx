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

import qualified Data.ByteString.Lazy.Char8    as L
import           Test.Hspec

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.InputOutput
import           Files

longestSequenceInFileBS :: L.ByteString
longestSequenceInFileBS = L.unlines $ map L.pack [ ">SEQUENCE_3"
                                                 , "ATTTAAAAAAACCCAAAACCCGGGCCCCGGGTTTTTTTA" ]

longestSequenceInFile :: Sequence
longestSequenceInFile = parseByteStringWith "Fasta byte string" (fastaSequence DNA) longestSequenceInFileBS

spec :: Spec
spec = do
  describe "longest" $
    it "finds the longest sequence"$ do
    ss <- parseFileWith (fasta DNA) fastaDifferentLengthFN
    longest ss `shouldBe` longestSequenceInFile

  describe "filterLongerThan" $
    it "filters sequences that are longer than a specified length" $ do
    ss  <- parseFileWith (fasta DNA) fastaDifferentLengthFN
    ss' <- parseFileWith (fasta DNA) fastaDifferentLengthTrimmedFN
    filterLongerThan 10 ss `shouldBe` ss'
