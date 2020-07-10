{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Data.Tree.MeasurableTree
-- Description :  Functions on trees with branch lengths
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 14:16:34 2019.
module ELynx.Data.Tree.MeasurableTree
  ( BranchLength,
    branchLength,
    fromBranchLengthUnsafe,
    Measurable (..),
    lengthenStem,
    setStem,
    distancesOriginLeaves,
    averageDistanceOriginLeaves,
    height,
    summarize,
    totalBranchLength,
    normalizeBranchLength,
    ultrametric,
  )
where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable
import Data.Tree
import ELynx.Data.Tree.Tree
import ELynx.Tools
import Text.Printf

-- | Branch length.
--
-- | Not all branches may have a length. For example, the stem may not have a
-- given length when reading a Newick file.
newtype BranchLength = BL {fromBranchLength :: Maybe Double}
  deriving (Eq, Ord, Read, Show)

instance Num BranchLength where
  l + r = BL $ liftA2 (+) (fromBranchLength l) (fromBranchLength r)
  l * r = BL $ liftA2 (*) (fromBranchLength l) (fromBranchLength r)
  abs = BL . fmap abs . fromBranchLength
  signum = BL . fmap signum . fromBranchLength
  fromInteger = BL . Just . fromInteger
  negate = error "negate: Branch lengths cannot be negative."

instance Fractional BranchLength where
  fromRational = BL . Just . fromRational
  l / r = BL $ liftA2 (/) (fromBranchLength l) (fromBranchLength r)

instance Floating BranchLength where
  pi = BL $ Just pi
  exp = BL . fmap exp . fromBranchLength
  log = BL . fmap log . fromBranchLength
  sin = BL . fmap sin . fromBranchLength
  cos = BL . fmap cos . fromBranchLength
  asin = BL . fmap asin . fromBranchLength
  acos = BL . fmap acos . fromBranchLength
  atan = BL . fmap atan . fromBranchLength
  sinh = BL . fmap sinh . fromBranchLength
  cosh = BL . fmap cosh . fromBranchLength
  asinh = BL . fmap asinh . fromBranchLength
  acosh = BL . fmap acosh . fromBranchLength
  atanh = BL . fmap atanh . fromBranchLength

-- | Conversion to branch length. Fail if given value is negative.
branchLength :: Maybe Double -> BranchLength
branchLength Nothing = BL Nothing
branchLength (Just l)
    | l >= 0 = BL (Just l)
    | otherwise = error $ "Branch lengths cannot be negative: " <> show l

-- | Conversion from branch length. Fail if branch length is not given.
fromBranchLengthUnsafe :: BranchLength -> Double
fromBranchLengthUnsafe (BL (Just x)) = x
fromBranchLengthUnsafe _ = error "fromBranchLengthUnsafe: Branch length not available."

pretty :: BranchLength -> String
pretty (BL Nothing) = "Nothing"
pretty (BL (Just x)) = printf "%.5f" x

-- | A 'Node' label with measurable and modifiable branch length to the parent.
class Measurable a where
  -- | Length of attached branch.
  getLen :: a -> BranchLength

  -- | Set attached branch length.
  setLen :: BranchLength -> a -> a

-- | Lengthen the stem of a tree.
lengthenStem :: Measurable a => BranchLength -> Tree a -> Tree a
lengthenStem dx t = setStem (dx + getLen (rootLabel t)) t

-- | Set the length of the stem of a tree.
setStem :: Measurable a => BranchLength -> Tree a -> Tree a
setStem x (Node l f) = Node (setLen x l) f

-- | Distances from the origin of a tree to its leaves (this is not the distance
-- from the root node to the leaves, which would be @distanceOriginLeaves t -
-- (getLen $ rootLabel t)@.). See also 'distancesRootLeaves'.
distancesOriginLeaves :: (Measurable a) => Tree a -> [BranchLength]
distancesOriginLeaves (Node l f) = map (getLen l +) (concatMap distancesOriginLeaves f)

-- | Average distance from the origin of a tree to its leaves, see
-- 'distancesOriginLeaves'.
averageDistanceOriginLeaves :: (Measurable a) => Tree a -> BranchLength
averageDistanceOriginLeaves tr = sum ds / fromIntegral n
  where
    ds = distancesOriginLeaves tr
    n = length ds

-- | Height (max distance between origin and leaves) of a tree. Return 0 if the
-- tree is empty.
height :: (Measurable a) => Tree a -> BranchLength
height = maximum . distancesOriginLeaves

pRow :: String -> String -> L.ByteString
pRow name val = alignLeft 33 n <> alignRight 8 v
  where
    n = L.pack name
    v = L.pack val

-- | Summarize a tree with measureable branch lengths.
summarize :: (Measurable a) => Tree a -> L.ByteString
summarize t =
  L.intercalate
    "\n"
    [ pRow "Leaves: " $ show n,
      pRow "Origin height: " $ pretty h,
      pRow "Average distance origin to leaves: " $ pretty h',
      pRow "Total branch length: " $ pretty b
    ]
  where
    n = length . leaves $ t
    h = height t
    h' = sum (distancesOriginLeaves t) / fromIntegral n
    b = totalBranchLength t

-- | Total branch length of a tree.
totalBranchLength :: (Measurable a) => Tree a -> BranchLength
totalBranchLength = foldl' (\acc n -> acc + getLen n) 0

-- | Normalize tree so that sum of branch lengths is 1.0.
normalizeBranchLength :: (Measurable a) => Tree a -> Tree a
normalizeBranchLength t = fmap (\n -> setLen (getLen n / s) n) t
  where
    s = totalBranchLength t

-- | Check if a tree is ultrametric.
ultrametric :: Measurable a => Tree a -> Maybe Bool
ultrametric = fmap allNearlyEqual . mapM fromBranchLength . distancesOriginLeaves
