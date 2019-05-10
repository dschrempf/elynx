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

import qualified Data.IntMap.Strict             as IntMap
import qualified Data.Map.Strict                as Map
import qualified Data.Vector.Unboxed            as V

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Alphabet.Character

codes :: [Code]
codes = [DNA, Protein]

alphabets :: [Alphabet]
alphabets = map alphabet codes

id' :: Code -> Character -> Character
id' code = (indexToCharacterMap code IntMap.!) . (characterToIndexMap code Map.!)

convertAlphabet :: Code -> Alphabet -> Alphabet
convertAlphabet code a = Alphabet $ V.map (id' code) a'
  where a' = fromAlphabet a

spec :: Spec
spec = describe "indexToCharacter . characterToIndex" $
  it "should be the identity" $
    zipWith convertAlphabet codes alphabets `shouldBe` alphabets
