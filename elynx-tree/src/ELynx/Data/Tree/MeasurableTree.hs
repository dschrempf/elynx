{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Data.Tree.MeasurableTree
Description :  Functions on trees with branch lengths
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 14:16:34 2019.

-}


module ELynx.Data.Tree.MeasurableTree
  ( Measurable (..)
  , distancesRootLeaves
  , averageDistanceRootLeaves
  , height
  , lengthenStem
  , shortenStem
  , summarize
  , totalBranchLength
  , normalize
  , prune
  ) where

import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Foldable
import           Data.Tree

import           ELynx.Data.Tree.Tree

-- | A 'Node' label with measurable and modifiable branch length to the parent.
class Measurable a where
  -- | Length of attached branch.
  getLen :: a -> Double
  -- | Set attached branch length.
  setLen :: Double -> a -> a

  -- | Elongate branch length.
  lengthen :: Double -> a -> a
  lengthen dl l = setLen (dl + getLen l) l

  -- | Shorten branch length.
  shorten :: Double -> a -> a
  shorten dl = lengthen (-dl)

-- | Distances from the root of a tree to its leaves.
distancesRootLeaves :: (Measurable a) => Tree a -> [Double]
distancesRootLeaves (Node l []) = [getLen l]
distancesRootLeaves (Node l f ) = concatMap (map (+ getLen l) . distancesRootLeaves) f

-- | Average distance from the root of a tree to its leaves.
averageDistanceRootLeaves :: (Measurable a) => Tree a -> Double
averageDistanceRootLeaves tr = sum ds / fromIntegral n
  where ds = distancesRootLeaves tr
        n  = length ds

-- | Height of a tree. Returns 0 if the tree is empty.
height :: (Measurable a) => Tree a -> Double
height = maximum . distancesRootLeaves

-- | Lengthen the distance between root and origin.
lengthenStem :: (Measurable a) => Double -> Tree a -> Tree a
lengthenStem dl (Node lbl chs) = Node (lengthen dl lbl) chs

-- | Lengthen the distance between root and origin.
shortenStem :: (Measurable a) => Double -> Tree a -> Tree a
shortenStem dl = lengthenStem (-dl)

-- | Summarize a tree with measureable branch lengths.
summarize :: (Measurable a) => Tree a -> L.ByteString
summarize t = L.intercalate "\n" $ map L.pack
  [ "Leaves: " ++ show n ++ "."
  , "Height: " ++ show h ++ "."
  , "Average distance root to leaves: " ++ show h' ++ "."
  , "Total branch length: " ++ show b ++ "." ]
  where n = length . leaves $ t
        h = height t
        b = totalBranchLength t
        h' = sum (distancesRootLeaves t) / fromIntegral n

-- | Total branch length of a tree.
totalBranchLength :: (Measurable a) => Tree a -> Double
totalBranchLength = foldl' (\acc n -> acc + getLen n) 0

-- | Normalize tree so that sum of branch lengths is 1.0.
normalize :: (Measurable a) => Tree a -> Tree a
normalize t = fmap (\n -> setLen (getLen n / s) n) t
  where s = totalBranchLength t

-- | Prune degree 2 nodes. Add branch lengths but forget pruned node label. See
-- 'pruneWith'.
prune :: (Measurable a) => Tree a -> Tree a
prune = pruneWith f
  where f da pa = lengthen (getLen pa) da
