{- |
Module      :  ELynx.Data.Tree.SubSample
Description :  Sub sample trees
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Feb 11 15:56:01 2020.

-}

module ELynx.Data.Tree.SubSample
  ( subSample
  , nSubSamples
  )
where

import           Control.Monad                  ( replicateM )
import           Control.Monad.Primitive
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
import           Data.Tree
import           System.Random.MWC

import           ELynx.Data.Tree.Tree           ( subTree )
import           ELynx.Tools.Random

-- | Extract a random subtree with @N@ leaves of a tree with @M@ leaves, where
-- @M>N@ (otherwise error). The complete list of leaves (names are assumed to be
-- unique) has to be provided as a 'Seq.Seq', and a 'Seq.Set', so that fast
-- sub-sampling as well as lookup are fast and so that these data structures do
-- not have to be recomputed when many sub-samples are requested.
subSample
  :: (PrimMonad m, Ord a)
  => Seq.Seq a
  -> Int
  -> Tree a
  -> Gen (PrimState m)
  -> m (Maybe (Tree a))
subSample lvs n tree g
  | Seq.length lvs < n = error
    "Given list of leaves is shorter than requested number of leaves."
  | otherwise = do
    sampledLs <- sample lvs n g
    let ls = Set.fromList sampledLs
    return $ subTree (`Set.member` ls) tree

-- | See 'subSample', but @n@ times.
nSubSamples
  :: (PrimMonad m, Ord a)
  => Int
  -> Seq.Seq a
  -> Int
  -> Tree a
  -> Gen (PrimState m)
  -> m [Maybe (Tree a)]
nSubSamples nS lvs nL tree g = replicateM nS $ subSample lvs nL tree g

