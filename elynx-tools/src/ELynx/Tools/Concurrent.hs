{-# LANGUAGE ScopedTypeVariables #-}
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
  , parComp
  , getChunks
  , parMapChunk
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
-- import qualified Control.Monad.Parallel      as P
import           Control.Monad.Primitive
import           Control.Parallel.Strategies
import qualified Data.Vector                 as V
import           Data.Word
import           System.Random.MWC

-- import           ELynx.Tools.Definitions

-- | Get a number of generators, possibly with a fixed seed.
getNGen :: Int -> Maybe [Word32] -> IO [GenIO]
getNGen n ms = do
  g <- maybe createSystemRandom (initialize . V.fromList) ms
  splitGen n g

-- | Split a generator.
splitGen :: PrimMonad m => Int -> Gen (PrimState m) -> m [Gen (PrimState m)]
splitGen n gen
  | n <= 0    = return []
  | otherwise = do
      seeds :: [V.Vector Word32] <- replicateM n $ uniformVector gen 256
      mapM initialize seeds

-- -- TODO: This just doesn't work... The only thing I found:
-- -- https://stackoverflow.com/a/16250010.
-- parComp :: (PrimMonad m, Monoid b) => Int -> (Int -> Gen (PrimState m) -> m b)
--         -> Gen (PrimState m) -> m b
-- parComp num fun gen = do
--   let ncap   = ceiling (fromIntegral num / fromIntegral chunksize :: Double)
--       chunks = getChunks ncap num
--   gs <- splitGen ncap gen
--   mconcat <$> P.mapM (\(n', g') -> fun n' g') (zip chunks gs)

-- | Perform random calculation in parallel. Does only work with 'IO' and the moment.
parComp :: Int -> (Int -> GenIO -> IO b) -> GenIO -> IO [b]
parComp num fun gen = do
  ncap   <- getNumCapabilities
  let chunks = getChunks ncap num
  gs <- splitGen ncap gen
  mapConcurrently (uncurry fun) (zip chunks gs)

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
