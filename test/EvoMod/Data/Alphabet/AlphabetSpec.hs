{- |
Module      :  EvoMod.Data.Alphabet.AlphabetSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 28 11:36:26 2019.

-}

module EvoMod.Data.Alphabet.AlphabetSpec
  ( spec
  ) where

import           Test.Hspec

import qualified Data.Vector.Storable          as V

import           EvoMod.Data.Alphabet.Alphabet

codes :: [Code]
codes = [DNA, DNA_IUPAC, Protein, ProteinIUPAC]

alphabets :: [Alphabet]
alphabets = map alphabet codes

convertAlphabet :: Code -> Alphabet -> Alphabet
convertAlphabet c a = Alphabet $ V.map (indexToCharacter c . characterToIndex c) a'
  where a' = fromAlphabet a

spec :: Spec
spec = describe "indexToCharacter . characterToIndex" $
  it "should be the identity" $
  zipWith convertAlphabet codes alphabets `shouldBe` alphabets
