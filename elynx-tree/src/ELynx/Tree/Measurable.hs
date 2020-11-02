{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Measurable
-- Description :  Measurable branch labels
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 14:16:34 2019.
--
-- Non-negativity of branch lengths is not (yet) ensured. To ensure
-- non-negativity, a newtype wrapper could be used, but this would be a major
-- refactor.
module ELynx.Tree.Measurable
  ( Length (fromLength),
    toLength,
    toLengthUnsafe,
    Measurable (..),
    applyMeasurable,
    height,
    rootHeight,
    distancesOriginLeaves,
    totalLength,
    normalizeLengths,
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

-- | Non-negative branch length.
--
-- However, non-negativity is only checked with 'toLength'. Negative values
-- can be obtained using the 'Num', 'Fractional', and 'Floating' instances.
newtype Length = Length {fromLength :: Double}
  deriving (Read, Show, Generic, NFData)
  deriving (Eq, Ord, Num, Floating, Fractional) via Double
  deriving (Semigroup, Monoid) via Sum Double

instance Splittable Length where
  split = (/ 2.0)

instance ToJSON Length

instance FromJSON Length

instance Measurable Length where
  getLen = id
  setLen = const

-- | Nothing if support is negative.
toLength :: Double -> Either String Length
toLength x | x < 0 = Left $ "length: Branch length is negative: " ++ show x ++ "."
         | otherwise = Right $ Length x

-- | Do not check if support value is negative.
toLengthUnsafe :: Double -> Length
toLengthUnsafe = Length

-- | A branch label with measurable and modifiable branch length.
class Measurable e where
  -- | Length of attached branch.
  getLen :: e -> Length

  -- | Set attached branch length.
  setLen :: Length -> e -> e

-- | Apply a function to a branch length label.
applyMeasurable :: Measurable e => (Length -> Length) -> e -> e
applyMeasurable f l = setLen (f s) l where s = getLen l

-- | The maximum distance between origin and leaves.
--
-- The height includes the length of the stem.
height :: Measurable e => Tree e a -> Length
height = maximum . distancesOriginLeaves

-- | The maximum distance between root node and leaves.
rootHeight :: Measurable e => Tree e a -> Length
rootHeight (Node _ _ []) = 0
rootHeight t = maximum $ concatMap distancesOriginLeaves (forest t)

-- | Distances from the origin of a tree to the leaves.
--
-- The distances include the length of the stem.
distancesOriginLeaves :: Measurable e => Tree e a -> [Length]
distancesOriginLeaves (Node br _ []) = [getLen br]
distancesOriginLeaves (Node br _ ts) = map (getLen br +) (concatMap distancesOriginLeaves ts)

-- | Total branch length of a tree.
totalLength :: Measurable e => Tree e a -> Length
totalLength = bifoldl' (+) const 0 . first getLen

-- | Normalize branch lengths so that the sum is 1.0.
normalizeLengths :: Measurable e => Tree e a -> Tree e a
normalizeLengths t = first (applyMeasurable (/ s)) t
  where
    s = totalLength t

-- | Normalize height of tree to 1.0.
normalizeHeight :: Measurable e => Tree e a -> Tree e a
normalizeHeight t = first (applyMeasurable (/ h)) t
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
ultrametric :: Measurable e => Tree e a -> Bool
ultrametric = allNearlyEqual . distancesOriginLeaves

-- | Elongate terminal branches such that the tree becomes ultrametric.
makeUltrametric :: Measurable e => Tree e a -> Tree e a
makeUltrametric t = go 0 t
  where
    h = height t
    go :: Measurable e => Length -> Tree e a -> Tree e a
    go h' (Node br lb []) = let dh = h - h' - getLen br in Node (applyMeasurable (+ dh) br) lb []
    go h' (Node br lb ts) = let h'' = h' + getLen br in Node br lb $ map (go h'') ts
