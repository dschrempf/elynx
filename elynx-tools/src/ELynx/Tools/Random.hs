{-# LANGUAGE BangPatterns #-}

{- |
Module      :  ELynx.Tools.Random
Description :  Tools for random sampling
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue May  7 11:22:12 2019.

-}

module ELynx.Tools.Random
  ( -- * Random
    sample
  )
where

import           Control.Monad.Primitive
import           Data.Foldable                  ( toList )
import qualified Data.Sequence                 as Seq
import           System.Random.MWC

-- | From a given sequence, randomly sample without replacement a number of elements.
sample :: PrimMonad m => Seq.Seq a -> Int -> Gen (PrimState m) -> m [a]
sample ys size = go 0 (l - 1) ys where
  l = Seq.length ys
  go !n !i xs g
    | n >= size = return $! (toList . Seq.drop (l - size)) xs
    | otherwise = do
      j <- uniformR (0, i) g
      let toI  = xs `Seq.index` j
          toJ  = xs `Seq.index` i
          next = (Seq.update i toI . Seq.update j toJ) xs
      go (n + 1) (i - 1) next g
{-# INLINE sample #-}
