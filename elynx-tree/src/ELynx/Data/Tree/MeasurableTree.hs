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

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable
import Data.Tree
import ELynx.Data.Tree.Tree
import ELynx.Tools
import Text.Printf

-- | Branch length.
type BranchLength = Double

pretty :: BranchLength -> String
pretty = printf "%.5f"

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
distancesOriginLeaves (Node l []) = [getLen l]
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
ultrametric :: Measurable a => Tree a -> Bool
ultrametric = allNearlyEqual . distancesOriginLeaves
