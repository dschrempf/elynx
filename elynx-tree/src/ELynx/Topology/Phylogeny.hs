-- |
-- Module      :  ELynx.Topology.Phylogeny
-- Description :  Phylogenetic topologies
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jul 18 13:15:49 2020.
--
-- A topology, as it is used in phylogenetics is a 'Topology' with unique leaf
-- labels, and the order of the topologies in the sub-forest is considered to be
-- meaningless.
--
-- NOTE: See the documentation of "ELynx.Tree.Phylogeny".
module ELynx.Topology.Phylogeny
  ( equal,
    equal',
    intersect,
    bifurcating,
    outgroup,
    midpoint,
    roots,
  )
where

import Data.List hiding (intersect)
import qualified Data.List.NonEmpty as N
import Data.Maybe
import qualified Data.Set as S
import ELynx.Topology.Rooted
import ELynx.Tree.Bipartition

-- | The equality check is slow because the order of children is considered to
-- be arbitrary.
--
-- Return 'Left' if a topology does not have unique leaves.
equal :: (Eq a, Ord a) => Topology a -> Topology a -> Either String Bool
equal tL tR
  | duplicateLeaves tL = Left "equal: Left topology has duplicate leaves."
  | duplicateLeaves tR = Left "equal: Right topology has duplicate leaves."
  | otherwise = Right $ equal' tL tR

-- | Same as 'equal', but assume that leaves are unique.
equal' :: Eq a => Topology a -> Topology a -> Bool
equal' (Leaf lbL) (Leaf lbR) =
  lbL == lbR
equal' (Node tsL) (Node tsR) =
  (length tsL == length tsR)
    && all (`elem'` tsR) tsL
  where
    elem' t ts = isJust $ find (equal' t) ts
equal' _ _ = False

-- | Intersection of topologies.
--
-- | See 'ELynx.Tree.Phylogeny.intersect'.
intersect ::
  (Ord a) => [Topology a] -> Either String [Topology a]
intersect ts
  | S.null lvsCommon = Left "intersect: Intersection of leaves is empty."
  | otherwise = case sequence [dropLeavesWith (predicate ls) t | (ls, t) <- zip leavesToDrop ts] of
    Nothing -> Left "intersect: A topology is empty."
    Just ts' -> Right ts'
  where
    -- Leaf sets.
    lvss = map (S.fromList . leaves) ts
    -- Common leaf set.
    lvsCommon = foldl1' S.intersection lvss
    -- Leaves to drop for each topology in the forest.
    leavesToDrop = map (S.\\ lvsCommon) lvss
    -- Predicate.
    predicate lvsToDr l = l `S.member` lvsToDr

-- | Check if topology is bifurcating.
--
-- | See 'ELynx.Tree.Phylogeny.intersect'.
bifurcating :: Topology a -> Bool
bifurcating (Leaf _) = True
bifurcating (Node ts) = (length ts == 2) && all bifurcating ts

-- | Root topology using an outgroup.
--
--   See 'ELynx.Tree.Phylogeny.outgroup'.
outgroup :: (Monoid a, Ord a) => S.Set a -> Topology a -> Either String (Topology a)
outgroup _ (Leaf _) = Left "outgroup: Root node is a leaf."
outgroup o t@(Node ts) = case N.toList ts of
  [] -> error "outgroup: Empty non-empty list?"
  [_] -> Left "outgroup: Root node has degree two."
  [_, _] -> do
    bip <- bp o (S.fromList (leaves t) S.\\ o)
    rootAt bip t
  xs -> outgroup o t'
    where
      tI = head xs
      -- Introduce a bifurcating root node.
      t' = Node $ N.fromList [tI, Node $ N.fromList (tail xs)]

-- | Root topology at the midpoint.
--
-- See 'ELynx.Tree.Phylogeny.midpoint'.
--
-- Use 'depth' to measure topology height.
--
-- If the midpoint is ambiguous because the sum of the left and right depths is
-- odd, the depth of the left sub-topology will be set to be one node greater
-- than the one of the right sub-topology.
midpoint :: Topology a -> Either String (Topology a)
midpoint (Leaf b) = Right $ Leaf b
midpoint t@(Node ts) = case N.toList ts of
  [] -> error "midpoint: Empty non-empty list?"
  [_] -> Left "midpoint: Root node has degree two."
  [_, _] -> roots t >>= getMidpoint
  _ -> Left "midpoint: Root node is multifurcating."

-- TODO.
getMidpoint :: [Topology a] -> Either String (Topology a)
getMidpoint = undefined

-- TODO.
roots :: Topology a -> Either String [Topology a]
roots = undefined

-- TODO.
rootAt :: (Eq a, Ord a) => Bipartition a -> Topology a -> Either String (Topology a)
rootAt = undefined

