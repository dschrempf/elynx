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

import           Data.Array.Repa               as R
import           Data.Functor.Identity
import qualified Data.Vector.Unboxed           as V
import           Data.Word

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Tools.Numeric

-- | Entropy of vector.
entropy :: Array D DIM1 Double -> Double
entropy arr = negate $ runIdentity $ sumAllP $ R.map xLogX arr

-- | Effective number of used characters measured using 'entropy'. The result
-- only makes sense when the sum of the array is 1.0.
kEffEntropy :: Array D DIM1 Double -> Double
kEffEntropy = exp . entropy

-- | Probability of homoplasy of vector. The result is the probability of
-- binomially sampling the same character twice and only makes sense when the
-- sum of the array is 1.0.
homoplasy :: Array D DIM1 Double -> Double
homoplasy arr = runIdentity $ sumAllP $ R.map (\x -> x*x) arr

-- | Effective number of used characters measured using 'homoplasy'. The result
-- only makes sense when the sum of the array is 1.0.
kEffHomoplasy :: Array D DIM1 Double -> Double
kEffHomoplasy arr = 1.0 / homoplasy arr

-- Increment element at index in vector by one.
incrementElemIndexByOne :: Int -> V.Vector Int -> V.Vector Int
incrementElemIndexByOne i v = v V.// [(i, e+1)]
  where e = v V.! i

countCharacters :: Code -> Array D DIM1 Word8 -> Array U DIM1 Int
countCharacters code d = fromUnboxed (ix1 nCharacters) $
  V.foldl' (\vec char -> incrementElemIndexByOne (characterToIndex code char) vec) zeroCounts v
  where
    v           = toUnboxed $ computeUnboxedS d
    nCharacters = cardinalityFromCode code
    zeroCounts  = V.replicate nCharacters (0 :: Int)

frequencyCharacters :: Code -> Array D DIM1 Word8 -> Array D DIM1 Double
frequencyCharacters code d = R.map (\e -> fromIntegral e / fromIntegral s) counts
  where
    counts = countCharacters code d
    s      = runIdentity $ sumAllP counts
