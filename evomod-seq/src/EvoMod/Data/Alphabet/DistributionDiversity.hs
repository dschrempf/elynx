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

-- import           Data.Functor.Identity
-- import qualified Data.Matrix.Storable          as M
import qualified Data.Map                       as Map
import qualified Data.Vector.Unboxed            as V

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Tools.Numeric
import           EvoMod.Tools.UVector

-- | Entropy of vector.
entropy :: V.Vector Double -> Double
entropy v = negate $ sumVec $ V.map xLogX v

-- | Effective number of used characters measured using 'entropy'. The result
-- only makes sense when the sum of the array is 1.0.
kEffEntropy :: V.Vector Double -> Double
-- kEffEntropy v = exp . entropy
kEffEntropy v = if e < 1e-8
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

-- -- Increment element at index in vector by one.
-- incrementElemIndexByOne :: [Int] -> V.Vector Int -> V.Vector Int
-- incrementElemIndexByOne is v = v V.// zip is es'
--   where es' = [v V.! i + 1 | i <- is]

-- XXX: Use mutable vector; then V.// is much faster.
-- Increment element at index in vector by one.
incrementElemIndexByOne :: [Int] -> V.Vector Int -> V.Vector Int
incrementElemIndexByOne is v = v V.// zip is es'
  where es' = [v V.! i + 1 | i <- is]

-- For a given code and counts vector, increment the count of the given character.
acc :: Code -> V.Vector Int -> Character -> V.Vector Int
acc code vec char = incrementElemIndexByOne is vec
  where
    charsNonIupac = iupacToStandard code char
    is            = map (characterToIndex code Map.!) charsNonIupac

countCharacters :: Code -> V.Vector Character -> V.Vector Int
countCharacters code =
  V.foldl' (acc code) zeroCounts
  where
    nChars     = cardinality code
    zeroCounts = V.replicate nChars (0 :: Int)

-- | For a given code vector of characters, calculate frequency of characters.
frequencyCharacters :: Code -> V.Vector Character -> V.Vector Double
frequencyCharacters code d = V.map (\e -> fromIntegral e / fromIntegral s) counts
  where
    counts = countCharacters code d
    s      = sumVec counts
