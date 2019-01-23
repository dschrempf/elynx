{- |
Module      :  EvoMod.Export.Sequence.FastaSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 18 09:59:57 2019.

-}

module EvoMod.Export.Sequence.FastaSpec
  (spec) where

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Export.Sequence.Fasta
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools
import           Files
import           Test.Hspec

spec :: Spec
spec =
  describe "sequencesToFasta" $
    it "should create a fasta bytestring that, when parsed again, is the original sequence" $ do
    ss <- parseFileWith (fastaFile DNA_IUPAC) fastaNucleotideIUPACFN
    let f   = sequencesToFasta ss
        ss' = parseByteStringWith (fastaFile DNA_IUPAC) f
    ss `shouldBe` ss'
