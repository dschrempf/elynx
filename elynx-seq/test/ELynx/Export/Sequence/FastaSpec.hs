{- |
Module      :  ELynx.Export.Sequence.FastaSpec
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 18 09:59:57 2019.

-}

module ELynx.Export.Sequence.FastaSpec
  ( spec
  )
where

import           Test.Hspec

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Export.Sequence.Fasta
import           ELynx.Import.Sequence.Fasta
import           ELynx.Tools

fastaNucleotideIUPACFN :: FilePath
fastaNucleotideIUPACFN = "data/NucleotideIUPAC.fasta"

spec :: Spec
spec =
  describe "sequencesToFasta"
    $ it
        "should create a fasta bytestring that, when parsed again, is the original sequence"
    $ do
        ss <- parseFileWith (fasta DNAI) fastaNucleotideIUPACFN
        let f   = sequencesToFasta ss
            ss' = parseByteStringWith
              "Fasta byte string created from sequence object"
              (fasta DNAI)
              f
        ss `shouldBe` ss'
