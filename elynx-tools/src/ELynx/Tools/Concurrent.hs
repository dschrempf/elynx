{- |
Module      :  ELynx.Tools.Concurrent
Description :  Tools for concurrent random calculations
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue May  7 10:33:24 2019.

-}

module ELynx.Tools.Concurrent
  (
    -- * MWC
    getNGen
  , splitGen
   -- * Parallel stuff
  , getChunks
  , parMapChunk
  ) where

import           Control.Monad
import           Control.Parallel.Strategies
import qualified Data.Vector                 as V
import           Data.Word
import           System.Random.MWC

-- | Get a number of generators, possibly with a fixed seed.
getNGen :: Int -> Maybe [Word32] -> IO [GenIO]
getNGen n ms = do
  g <- maybe createSystemRandom (initialize . V.fromList) ms
  splitGen n g

-- | Split a generator.
splitGen :: Int -> GenIO -> IO [GenIO]
splitGen n gen
  | n <= 0    = return []
  | otherwise =
  fmap (gen:) . replicateM (n-1) $
  initialize =<< (uniformVector gen 256 :: IO (V.Vector Word32))

-- | For a given number of capabilities and number of calculations, get chunk
-- sizes. The chunk sizes will be as evenly distributed as possible and sum up
-- to the number of calculations.
getChunks :: Int -> Int -> [Int]
getChunks c n = ns
  where n'  = n `div` c
        r = n `mod` c
        ns = replicate r (n'+1) ++ replicate (c - r) n'

-- | Parallel map with given chunk size.
parMapChunk :: Int -> (a -> b) -> [a] -> [b]
parMapChunk n f as = map f as `using` parListChunk n rseq
