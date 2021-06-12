{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  ELynx.Tools.Concurrent
-- Description :  Tools for concurrent random calculations
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue May  7 10:33:24 2019.
module ELynx.Tools.Concurrent
  ( -- * MWC
    splitGen,

    -- * Concurrent calculations
    getChunks,
  )
where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector as V
import Data.Word
import System.Random.MWC

-- | Split a generator.
splitGen :: PrimMonad m => Int -> Gen (PrimState m) -> m [Gen (PrimState m)]
splitGen n gen
  | n <= 0 = return []
  | otherwise = do
    seeds :: [V.Vector Word32] <- replicateM (n -1) $ uniformVector gen 256
    fmap (gen :) (mapM initialize seeds)

-- | For a given number of capabilities and number of calculations, get chunk
-- sizes. The chunk sizes will be as evenly distributed as possible and sum up
-- to the number of calculations.
getChunks :: Int -> Int -> [Int]
getChunks c n = ns
  where
    n' = n `div` c
    r = n `mod` c
    ns = replicate r (n' + 1) ++ replicate (c - r) n'
