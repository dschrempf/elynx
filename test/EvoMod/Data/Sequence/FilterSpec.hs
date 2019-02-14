{- |
Module      :  EvoMod.Data.Sequence.FilterSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 18 09:47:35 2019.

-}

module EvoMod.Data.Sequence.FilterSpec
  (spec) where

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.Filter
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.InputOutput
import           Files
import           Test.Hspec


spec :: Spec
spec =
  describe "filterLongerThan" $
    it "filters sequences that are longer than a specified length" $ do
    ss  <- parseFileWith (fasta DNA) fastaDifferentLengthFN
    ss' <- parseFileWith (fasta DNA) fastaDifferentLengthTrimmedFN
    filterLongerThan 10 ss `shouldBe` ss'
