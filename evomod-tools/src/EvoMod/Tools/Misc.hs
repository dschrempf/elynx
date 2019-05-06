{- |
Module      :  EvoMod.Tools.Misc
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:32:19 2019.

Miscellaneous tools that do not have their own category (yet).

-}

module EvoMod.Tools.Misc
  (
    -- * Not yet classified stuff
    ensure
    -- * Weird stuff
  , compose
  , allValues
  , horizontalConcat
    -- * MWC
  , splitGen
   -- * Parallel stuff
  , parMapChunk
  ) where

import           Control.Monad
import           Control.Parallel.Strategies
import           Data.List
import qualified Data.Vector                 as V
import           Data.Word
import           System.Random.MWC

-- | Chain a list of functions together. See https://wiki.haskell.org/Compose.
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

-- | Get all values of a bounded enumerated type.
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

-- | A brain f***. As an example, let @xss@ be a list of alignments (i.e., a
-- list of a list of a list of alleles). This function horizontally concatenates
-- the sites. The number of species needs to be same in each alignment. No
-- checks are performed!
horizontalConcat :: [[[a]]] -> [[a]]
horizontalConcat [xs] = xs
horizontalConcat xss  = foldl' (zipWith (++)) (head xss) (tail xss)

-- | Should be in the library...
splitGen :: Int -> GenIO -> IO [GenIO]
splitGen n gen
  | n <= 0    = return []
  | otherwise =
  fmap (gen:) . replicateM (n-1) $
  initialize =<< (uniformVector gen 256 :: IO (V.Vector Word32))

-- | Parallel map with given chunk size.
parMapChunk :: Int -> (a -> b) -> [a] -> [b]
parMapChunk n f as = map f as `using` parListChunk n rseq

-- | Ensure that a value satisfies a given predicate.
ensure :: (a -> Bool) -> a -> Maybe a
ensure p v | p v       = Just v
           | otherwise = Nothing
