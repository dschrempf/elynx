{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  ELynx.Tree.Bipartition
-- Description :  Bipartitions on trees
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 30 15:28:17 2019.
module ELynx.Tree.Bipartition
  ( groups,

    -- * Data type
    Bipartition (fromBipartition),
    bp,
    bpUnsafe,
    bpMap,
    toSet,
    bpHuman,

    -- * Work with 'Bipartition's
    bipartition,
    bipartitions,
    getComplementaryLeaves,
    bipartitionToBranchLength,
  )
where

-- TODO: REFACTOR: CHECK THIS MODULE AGAIN.

import Control.Comonad
import Control.DeepSeq
import Data.List hiding (partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import ELynx.Tree.Length
import ELynx.Tree.Name
import ELynx.Tree.Rooted

-- | A bipartition of a tree is a grouping of the leaves of the tree into two
-- non-overlapping, non-empty sub sets.
--
-- For unrooted trees:
--
-- - Each branch partitions the leaves of the tree into two subsets, or a
--   bipartition.
--
-- For rooted trees:
--
-- - A bifurcating root node induces a bipartition; see 'bipartition'.
--
-- - Each node induces a bipartition by taking the leaves of the induces subtree
--   and the complement leaf set of the full tree.
--
-- The order of the two subsets of a 'Bipartition' is meaningless. That is,
-- 'Bipartition's are weird in that
--
-- > Bipartition x y == Bipartition y x
--
-- is 'True'. Also,
--
-- > Bipartition x y > Bipartition y x
--
-- is False, even when @x > y@. That's why we have to make sure that for
--
-- > Bipartition x y
--
-- we always have @x >= y@. We ensure by construction that the larger subset
-- comes first, and so that equality checks are meaningful; see 'bp' and
-- 'bpUnsafe'.
newtype Bipartition a = Bipartition
  { fromBipartition :: (Set a, Set a)
  }
  deriving (Eq, Ord, Show, Read, NFData)

-- | Each node of a tree is root of an induced subtree. Set the node labels to
-- the leaves of the induced subtrees.
groups :: Tree a -> Tree [a]
-- I am proud of this awesome 'Comonad' usage here :).
groups = extend leaves

-- | Create a bipartition from two sets.
--
-- Ensure that the larger set comes first.
--
-- Return 'Left' if one set is empty.
bp :: Ord a => Set a -> Set a -> Either String (Bipartition a)
bp xs ys
  | S.null xs = Left "bp: Left set empty."
  | S.null ys = Left "bp: Right set empty."
  | otherwise = Right $ bpUnsafe xs ys

-- | Create a bipartition from two sets.
--
-- Ensure that the larger set comes first.
bpUnsafe :: Ord a => Set a -> Set a -> Bipartition a
bpUnsafe xs ys = if xs >= ys then Bipartition (xs, ys) else Bipartition (ys, xs)

-- | Map a function over all elements in the 'Bipartition'.
bpMap :: Ord b => (a -> b) -> Bipartition a -> Bipartition b
bpMap f (Bipartition (x, y)) = bpUnsafe (S.map f x) (S.map f y)

-- | Conversion to a set containing both partitions.
toSet :: Ord a => Bipartition a -> Set a
toSet (Bipartition (x, y)) = S.union x y

-- I decided not to provide a human readable show instance because I need the
-- following identity to hold:
--
-- > read . show = id
--
-- This identity is met by the derived instance anyways. A more human readable
-- instance would most likely violate the identity. However, I provide functions
-- to convrt bipartitions into human readable strings.

-- | Show a bipartition in a human readable format. Use a provided function to
-- extract information of interest.
bpHuman :: Show a => Bipartition a -> String
bpHuman (Bipartition (x, y)) = "(" ++ setShow x ++ "|" ++ setShow y ++ ")"

-- Show the elements of a set in a human readable format.
setShow :: Show a => Set a -> String
setShow = intercalate "," . map show . S.toList

-- | For a bifurcating root, get the bipartition induced by the root node.
--
-- Return 'Left' if
-- - the root node is not bifurcating;
-- - a leave set is empty.
bipartition :: Ord a => Tree a -> Either String (Bipartition a)
bipartition (Node _ [x, y]) = bp (S.fromList $ leaves x) (S.fromList $ leaves y)
bipartition _ = Left "bipartition: Root node is not bifurcating."

-- | Get all bipartitions of the tree.
--
-- Return 'Left' if the tree contains duplicate leaves.
bipartitions :: (Ord a) => Tree a -> Either String (Set (Bipartition a))
bipartitions t
  | duplicateLeaves t = Left "bipartitions: Tree contains duplicate leaves."
  | otherwise = Right $ bipartitions' S.empty $ S.fromList <$> groups t

-- | Report the complementary leaves for each child.
getComplementaryLeaves ::
  (Ord a) =>
  -- Complementary leaves.
  Set a ->
  -- Tree with node labels storing leaves.
  Tree (Set a) ->
  [Set a]
getComplementaryLeaves p (Node _ ts) =
  [ S.unions $ p : take i lvsChildren ++ drop (i + 1) lvsChildren
    | i <- [0 .. (n -1)]
  ]
  where
    n = length ts
    lvsChildren = map label ts

-- See 'bipartitions', but do not check if leaves are unique, nor if
-- bipartitions are valid.
bipartitions' :: Ord a => Set a -> Tree (Set a) -> Set (Bipartition a)
bipartitions' p (Node p' []) = either (const S.empty) S.singleton $ bp p p'
bipartitions' p t@(Node p' ts) =
  S.unions $
    either (const S.empty) S.singleton (bp p p') :
      [bipartitions' c s | (c, s) <- zip cs ts]
  where
    cs = getComplementaryLeaves p t

-- | Convert a tree into a 'Map' from each 'Bipartition' defined by the node
-- names to the length of the branch inducing the respective 'Bipartition'.
--
-- Since the induced bipartitions of the daughter branches of a bifurcating root
-- node are equal, the branches leading to the root are combined. See
-- http://evolution.genetics.washington.edu/phylip/doc/treedist.html and how
-- unrooted trees are handled.
--
-- Further, branches connected to degree two nodes also induce the same
-- bipartitions and are combined.
--
-- Return 'Left' if the tree contains duplicate leaves.
bipartitionToBranchLength ::
  (HasName a, HasLength a) =>
  Tree a ->
  Either String (Map (Bipartition Name) Length)
bipartitionToBranchLength tr
  | duplicateLeaves trN = Left "bipartitionToBranchLength: Tree contains duplicate leaf names."
  | otherwise = Right $ bipartitionToBranchLength' S.empty trL pTr
  where
    trL = getLength <$> tr
    trN = getName <$> tr
    pTr = S.fromList <$> groups (getName <$> tr)

-- When calculating the map, branches separated by various degree two nodes have
-- to be combined. Hence, not only the complementary leaves, but also the branch
-- label itself have to be passed along.
bipartitionToBranchLength' ::
  -- Complementary leaves.
  Set Name ->
  -- Original tree.
  Tree Length ->
  -- Partition tree.
  Tree (Set Name) ->
  Map (Bipartition Name) Length
bipartitionToBranchLength' p tr pTr =
  M.unionsWith (<>) $
    either (const M.empty) (`M.singleton` lb) (bp p p') :
      [bipartitionToBranchLength' c tr' pTr' | (c, tr', pTr') <- zip3 cs trs pTrs]
  where
    lb = label tr
    p' = label pTr
    trs = forest tr
    pTrs = forest pTr
    cs = getComplementaryLeaves p pTr
