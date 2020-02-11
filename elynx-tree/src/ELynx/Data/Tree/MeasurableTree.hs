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
  ( Measurable(..)
  , extend
  , distancesRootLeaves
  , distancesOriginLeaves
  , averageDistanceOriginLeaves
  , height
  , rootHeight
  , lengthenStem
  , shortenStem
  , summarize
  , totalBranchLength
  , normalize
  , prune
  , removeMultifurcations
  , ultrametric
  )
where

import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Foldable
import           Data.Tree

import           ELynx.Data.Tree.Tree
import           ELynx.Tools.Equality           ( allNearlyEqual )

-- | A 'Node' label with measurable and modifiable branch length to the parent.
class Measurable a where
  -- | Length of attached branch.
  getLen :: a -> Double
  -- | Set attached branch length.
  setLen :: Double -> a -> a

lengthen :: Measurable a => Double -> a -> a
lengthen dl l = setLen (dl + getLen l) l

-- | @extend daughter parent@ takes the daughter node and extends the branch by
-- the length obtained from the parent node.
extend :: Measurable a => a -> a -> a
extend da pa = lengthen (getLen pa) da

-- | Elongate branch length.
-- -- | Shorten branch length.
-- shorten :: Double -> a -> a
-- shorten dl = lengthen (-dl)

-- | Distances from the root node of a tree to its leaves (this are not the
-- distances from the origin to the leaves, see 'distancesOriginLeaves').
distancesRootLeaves :: (Measurable a) => Tree a -> [Double]
distancesRootLeaves (Node _ []) = [0]
distancesRootLeaves (Node _ f) =
  concat [ map (+ getLen (rootLabel d)) (distancesRootLeaves d) | d <- f ]

-- -- | Distances from the origin of a tree to its leaves (this is not the distance
-- -- from the root node to the leaves, which would be @distanceOriginLeaves t -
-- -- (getLen $ rootLabel t)@.).
-- distancesOriginLeaves :: (Measurable a) => Tree a -> [Double]
-- distancesOriginLeaves (Node l []) = [getLen l]
-- distancesOriginLeaves (Node l f ) = concatMap (map (+ getLen l) . distancesOriginLeaves) f

-- | Distances from the origin of a tree to its leaves (this is not the distance
-- from the root node to the leaves, which would be @distanceOriginLeaves t -
-- (getLen $ rootLabel t)@.). See also 'distancesRootLeaves'.
distancesOriginLeaves :: (Measurable a) => Tree a -> [Double]
distancesOriginLeaves t@(Node l _) = map (+ getLen l) (distancesRootLeaves t)

-- | Average distance from the origin of a tree to its leaves, see
-- 'distancesOriginLeaves'.
averageDistanceOriginLeaves :: (Measurable a) => Tree a -> Double
averageDistanceOriginLeaves tr = sum ds / fromIntegral n
 where
  ds = distancesOriginLeaves tr
  n  = length ds

-- | Height (max distance between origin and leaves) of a tree. Return 0 if the
-- tree is empty.
height :: (Measurable a) => Tree a -> Double
height = maximum . distancesOriginLeaves

-- | Height of root node. Return 0 if the tree is empty.
rootHeight :: (Measurable a) => Tree a -> Double
rootHeight = maximum . distancesRootLeaves

-- | Lengthen the distance between root and origin.
lengthenStem :: (Measurable a) => Double -> Tree a -> Tree a
lengthenStem dl (Node lbl chs) = Node (lengthen dl lbl) chs

-- | Lengthen the distance between root and origin.
shortenStem :: (Measurable a) => Double -> Tree a -> Tree a
shortenStem dl = lengthenStem (-dl)

-- | Summarize a tree with measureable branch lengths.
summarize :: (Measurable a) => Tree a -> L.ByteString
summarize t = L.intercalate "\n" $ map
  L.pack
  [ "Leaves: " ++ show n ++ "."
  , "Height: " ++ show h ++ "."
  , "Average distance root to leaves: " ++ show h' ++ "."
  , "Total branch length: " ++ show b ++ "."
  ]
 where
  n  = length . leaves $ t
  h  = height t
  b  = totalBranchLength t
  h' = sum (distancesOriginLeaves t) / fromIntegral n

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
prune = pruneWith f where f da pa = lengthen (getLen pa) da

-- | Remove multifurcations by copying multifurcating nodes and introducing
-- branches of length 0.
removeMultifurcations :: Measurable a => Tree a -> Tree a
removeMultifurcations t@(Node _ [] ) = t
removeMultifurcations (  Node l [x]) = Node l [removeMultifurcations x]
removeMultifurcations (Node l [x, y]) =
  Node l $ map removeMultifurcations [x, y]
removeMultifurcations (Node l (x : xs)) = Node l
  $ map removeMultifurcations [x, Node l' xs]
  where l' = setLen 1.0 l

-- | Check if a tree is ultrametric.
ultrametric :: Measurable a => Tree a -> Bool
ultrametric = allNearlyEqual . distancesOriginLeaves
