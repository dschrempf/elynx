{- |
Module      :  EvoMod.Data.Alphabet.DistributionDiversity
Description :  Summarize statistics for alphabets
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon Feb 25 13:32:56 2019.

-}

module EvoMod.Data.Alphabet.DistributionDiversity
  ( -- * Entropy
    entropy
  , kEffEntropy
    -- * Homoplasy
  , homoplasy
  , kEffHomoplasy
    -- * Count characters
  , frequencyCharacters
  ) where

import qualified Data.Set                       as S
import qualified Data.Vector.Unboxed            as V

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Tools.Numeric
import           EvoMod.Tools.Vector
import           EvoMod.Tools.Definitions

-- | Entropy of vector.
entropy :: V.Vector Double -> Double
entropy v = if isNaN res
  then error ("entropy: Sesult of following vector is NaN: " ++ show v ++ ".")
  else res
  where res = negate $ sumVec $ V.map xLogX v

-- | Effective number of used characters measured using 'entropy'. The result
-- only makes sense when the sum of the array is 1.0.
kEffEntropy :: V.Vector Double -> Double
kEffEntropy v = if e < eps
                then 1.0
                else exp e
  where e = entropy v

-- | Probability of homoplasy of vector. The result is the probability of
-- binomially sampling the same character twice and only makes sense when the
-- sum of the array is 1.0.
homoplasy :: V.Vector Double -> Double
homoplasy v = sumVec $ V.map (\x -> x*x) v

-- | Effective number of used characters measured using 'homoplasy'. The result
-- only makes sense when the sum of the array is 1.0.
kEffHomoplasy :: V.Vector Double -> Double
kEffHomoplasy v = 1.0 / homoplasy v

-- XXX: Use mutable vector; then V.// is much faster.
-- Increment element at index in vector by one.
incrementElemIndexByOne :: [Int] -> V.Vector Int -> V.Vector Int
incrementElemIndexByOne is v = v V.// zip is es'
  where es' = [v V.! i + 1 | i <- is]

-- For a given code and counts vector, increment the count of the given character.
acc :: AlphabetSpec -> V.Vector Int -> Character -> V.Vector Int
acc alph vec char = incrementElemIndexByOne is vec
  where
    is = [ S.findIndex c (std alph) | c <- toStd alph char ]

countCharacters :: AlphabetSpec -> V.Vector Character -> V.Vector Int
countCharacters alph =
  V.foldl' (acc alph) zeroCounts
  where
    nChars     = length (std alph)
    zeroCounts = V.replicate nChars (0 :: Int)

saveDivision :: Int -> Int -> Double
saveDivision value divisor =
  if divisor == 0
  then 0.0
  else fromIntegral value / fromIntegral divisor

-- | For a given code vector of characters, calculate frequency of characters.
frequencyCharacters :: AlphabetSpec -> V.Vector Character -> V.Vector Double
frequencyCharacters alph d = V.map (`saveDivision` s) counts
  where
    counts = countCharacters alph d
    s      = sumVec counts
