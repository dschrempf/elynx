{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Length
-- Description :  Measurable labels
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 14:16:34 2019.
--
-- Non-negativity of lengths is not completely ensured. See the documentation of
-- 'Length'.
module ELynx.Tree.Length
  ( -- * Non-negative length
    Length (fromLength),
    toLength,
    toLengthUnsafe,
    HasLength (..),
    height,
    rootHeight,

    -- * Functions on trees
    distancesOriginLeaves,
    totalBranchLength,
    normalizeBranchLengths,
    normalizeHeight,
    ultrametric,
    makeUltrametric,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Semigroup
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import GHC.Generics

-- | Non-negative length.
--
-- However, non-negativity is only checked with 'toLength', and negative values
-- can be obtained using the 'Num' and related instances.
--
-- Safe conversion is roughly 50 percent slower.
--
-- @
-- benchmarking length/length sum foldl' with safe conversion
-- time                 110.4 ms   (109.8 ms .. 111.0 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 110.2 ms   (110.0 ms .. 110.6 ms)
-- std dev              501.8 μs   (359.1 μs .. 730.0 μs)
--
-- benchmarking length/length sum foldl' num instance
-- time                 89.37 ms   (85.13 ms .. 94.27 ms)
--                      0.996 R²   (0.992 R² .. 1.000 R²)
-- mean                 86.53 ms   (85.63 ms .. 88.52 ms)
-- std dev              2.239 ms   (1.069 ms .. 3.421 ms)
--
-- benchmarking length/double sum foldl'
-- time                 85.47 ms   (84.88 ms .. 86.42 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 85.56 ms   (85.26 ms .. 86.02 ms)
-- std dev              611.9 μs   (101.5 μs .. 851.7 μs)
-- @
newtype Length = Length {fromLength :: Double}
  deriving (Read, Show, Generic, NFData)
  deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real, RealFloat, RealFrac) via Double
  deriving (Semigroup, Monoid) via Sum Double

instance Splittable Length where
  split = (/ 2.0)

instance ToJSON Length

instance FromJSON Length

instance HasLength Length where
  getLen = id
  setLen = const
  modLen f = f

-- | Return 'Left' if negative.
toLength :: Double -> Either String Length
toLength x
  | x < 0 = Left $ "Length is negative: " ++ show x ++ "."
  | otherwise = Right $ Length x

-- | Do not check if value is negative.
toLengthUnsafe :: Double -> Length
toLengthUnsafe = Length

-- | A data type with measurable and modifiable values.
class HasLength e where
  -- | Get length.
  getLen :: e -> Length

  -- | Set length.
  setLen :: Length -> e -> e

  -- For computational efficiency.

  -- | Modify length.
  modLen :: (Length -> Length) -> e -> e

-- | The maximum distance between origin and leaves.
--
-- The height includes the branch length of the stem.
height :: HasLength e => Tree e a -> Length
height = maximum . distancesOriginLeaves

-- | The maximum distance between root node and leaves.
rootHeight :: HasLength e => Tree e a -> Length
rootHeight (Node _ _ []) = 0
rootHeight t = maximum $ concatMap distancesOriginLeaves (forest t)

-- | Distances from the origin of a tree to the leaves.
--
-- The distances include the branch length of the stem.
distancesOriginLeaves :: HasLength e => Tree e a -> [Length]
distancesOriginLeaves (Node br _ []) = [getLen br]
distancesOriginLeaves (Node br _ ts) = map (getLen br +) (concatMap distancesOriginLeaves ts)

-- | Total branch length of a tree.
totalBranchLength :: HasLength e => Tree e a -> Length
totalBranchLength = bifoldl' (+) const 0 . first getLen

-- | Normalize branch lengths so that the sum is 1.0.
normalizeBranchLengths :: HasLength e => Tree e a -> Tree e a
normalizeBranchLengths t = first (modLen (/ s)) t
  where
    s = totalBranchLength t

-- | Normalize height of tree to 1.0.
normalizeHeight :: HasLength e => Tree e a -> Tree e a
normalizeHeight t = first (modLen (/ h)) t
  where
    h = height t

eps :: Double
eps = 1e-12

allNearlyEqual :: [Length] -> Bool
allNearlyEqual [] = True
allNearlyEqual xs = all (\y -> eps > abs (fromLength $ x - y)) (tail xs)
  where
    x = head xs

-- | Check if a tree is ultrametric.
ultrametric :: HasLength e => Tree e a -> Bool
ultrametric = allNearlyEqual . distancesOriginLeaves

-- | Elongate terminal branches such that the tree becomes ultrametric.
makeUltrametric :: HasLength e => Tree e a -> Tree e a
makeUltrametric t = go 0 t
  where
    h = height t
    go :: HasLength e => Length -> Tree e a -> Tree e a
    go h' (Node br lb []) = let dh = h - h' - getLen br in Node (modLen (+ dh) br) lb []
    go h' (Node br lb ts) = let h'' = h' + getLen br in Node br lb $ map (go h'') ts
