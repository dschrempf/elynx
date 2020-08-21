-- |
-- Module      :  ELynx.Data.Tree.Measurable
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
module ELynx.Data.Tree.Measurable
  ( BranchLength,
    Measurable (..),
    applyStem,
    getStem,
    setStem,
    height,
    rootHeight,
    distancesOriginLeaves,
    totalBranchLength,
    normalizeBranchLengths,
    normalizeHeight,
    ultrametric,
    makeUltrametric,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import ELynx.Data.Tree.Rooted

-- | Branch length.
type BranchLength = Double

-- | A branch label with measurable and modifiable branch length.
class Measurable e where
  -- | Length of attached branch.
  getLen :: e -> BranchLength

  -- | Set attached branch length.
  setLen :: BranchLength -> e -> e

instance Measurable Double where
  getLen = id
  setLen = const

-- Apply a function to a branch support label.
apply :: Measurable e => (BranchLength -> BranchLength) -> e -> e
apply f l = setLen (f s) l where s = getLen l

-- | Lengthen the stem of a tree.
applyStem :: Measurable e => (BranchLength -> BranchLength) -> Tree e a -> Tree e a
applyStem f t = t {branch = apply f b}
  where
    b = branch t

-- | Get the length of the stem of a tree.
getStem :: Measurable e => Tree e a -> BranchLength
getStem (Node br _ _) = getLen br

-- | Set the length of the stem of a tree.
setStem :: Measurable e => BranchLength -> Tree e a -> Tree e a
setStem x = applyStem (const x)

-- | The maximum distance between origin and leaves.
--
-- The height includes the length of the stem.
height :: Measurable e => Tree e a -> BranchLength
height = maximum . distancesOriginLeaves

-- | The maximum distance between root node and leaves.
rootHeight :: Measurable e => Tree e a -> BranchLength
rootHeight (Node _ _ []) = 0
rootHeight t = maximum $ concatMap distancesOriginLeaves (forest t)

-- | Distances from the origin of a tree to the leaves.
--
-- The distances include the length of the stem.
distancesOriginLeaves :: Measurable e => Tree e a -> [BranchLength]
distancesOriginLeaves (Node br _ []) = [getLen br]
distancesOriginLeaves (Node br _ ts) = map (getLen br +) (concatMap distancesOriginLeaves ts)

-- | Total branch length of a tree.
totalBranchLength :: Measurable e => Tree e a -> BranchLength
totalBranchLength = bifoldl' (+) const 0 . first getLen

-- | Normalize branch lengths so that the sum is 1.0.
normalizeBranchLengths :: Measurable e => Tree e a -> Tree e a
normalizeBranchLengths t = first (apply (/ s)) t
  where
    s = totalBranchLength t

-- | Normalize height of tree to 1.0.
normalizeHeight :: Measurable e => Tree e a -> Tree e a
normalizeHeight t = first (apply (/ h)) t
  where
    h = height t

eps :: Double
eps = 1e-12

allNearlyEqual :: [Double] -> Bool
allNearlyEqual [] = True
allNearlyEqual xs = all (\y -> eps > abs (x - y)) (tail xs)
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
    go :: Measurable e => BranchLength -> Tree e a -> Tree e a
    go h' (Node br lb []) = let dh = h - h' - getLen br in Node (apply (+ dh) br) lb []
    go h' (Node br lb ts) = let h'' = h' + getLen br in Node br lb $ map (go h'') ts
