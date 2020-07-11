-- |
-- Module      :  ELynx.Data.Tree.PhyloTree
-- Description :  Phylogenetic trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 16:08:54 2019.
--
-- Phylogenetic nodes labels, aka 'PhyloLabel's, have a branch length and an
-- arbitrary label type, e.g., of type 'Int'.
module ELynx.Data.Tree.PhyloTree
  ( PhyloLabel (..),
    Length (..),
    toLength,
    Support (..),
    toSupport,
    roots,
    rootAt,
    connect,
    removeMultifurcations,
    extend,
    prune,
  )
where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Tree
import ELynx.Data.Tree.Bipartition
import ELynx.Data.Tree.BranchSupportTree
import ELynx.Data.Tree.MeasurableTree
import ELynx.Data.Tree.Tree
import ELynx.Data.Tree.NamedTree

-- | A label type for phylogenetic trees possibly with branch length and branch support.
data PhyloLabel a = PhyloLabel
  { pLabel :: a,
    pBrLen :: Maybe BranchLength,
    pBrSup :: Maybe BranchSupport
  }
  deriving (Read, Show, Eq, Ord)

instance Named a => Named (PhyloLabel a) where
  getName = getName . pLabel

-- | A label with a branch length.
data Length a = Length
  { lLabel :: a,
    brLen :: BranchLength
  }
  deriving (Read, Show, Eq, Ord)

instance Named a => Named (Length a) where
  getName = getName . lnLabel

instance Measurable (Length a) where
  getLen = brLen
  setLen x l = l {brLen = x}

-- | A label with branch support.
data Support a = Support
  { sLabel :: a,
    brSup :: BranchSupport
  }
  deriving (Read, Show, Eq, Ord)

instance Named a => Named (Support a) where
  getName = getName . spLabel

instance BranchSupported (Support a) where
  getBranchSupport = brSup
  setBranchSupport x l = l {brSup = x}

getMaxBrSup :: Tree (PhyloLabel a) -> BranchSupport
getMaxBrSup =
  -- If all branch support values are below 1.0, set the max support to 1.0.
  max 1.0 .
  -- If no branch support is given, set max support to 1.0.
  fromMaybe 1.0 . maximum . fmap pBrSup

-- | If root branch length is not available, set it to 0. Return 'Nothing' if
-- any other branch length is unavailable.
toLength :: Tree (PhyloLabel a) -> Maybe (Tree (Length a))
toLength = traverse toLengthLabel . cleanRootLength

cleanRootLength :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
cleanRootLength (Node (PhyloLabel l Nothing s) f) = Node (PhyloLabel l (Just 0) s)
cleanRootLength _ = id

toLengthLabel :: PhyloLabel a -> Maybe (Length a)
toLengthLabel (PhyloLabel l Nothing _) = Nothing
toLengthLabel (PhyloLabel l (Just b) _) = Length l b

-- | Set branch support values of branches leading to the leaves to maximum
-- support. Set the root branch support to maximum support.
--
-- Return 'Nothing' if any other branch has no available support value.
toSupport :: Tree (PhyloLabel a) -> Maybe (Tree (Support a))
toSupport t = traverse toSupportLabel . cleanLeafSupport m . cleanRootSupport m
  where
    m = getMaxBrSup t

cleanRootSupport :: BranchSupport -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
cleanRootSupport maxSup (Node (PhyloLabel l b Nothing) xs) = Node (PhyloLabel l (Just maxSup)) xs
cleanRootSupport _ t = t

cleanLeafSupport :: BranchSupport -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
cleanLeafSupport maxSup (Node (PhyloLabel l b Nothing) []) = Node (PhyloLabel l (Just maxSup)) []
cleanLeafSupport _ (Node l xs) = Node l $ map cleanLeafSupport xs

toSupportLabel :: PhyloLabel a -> Maybe (Support a)
toSupportLabel (PhyloLabel l _ Nothing) = Nothing
toSupportLabel (PhyloLabel l _ (Just s)) = Support l s

-- TODO. Go on here.

halve :: PhyloLabel a -> PhyloLabel a
halve l = setLen (getLen l / 2.0) l

-- | For a rooted, bifurcating tree, get all possible rooted trees.
--
-- For a tree with @n>2@ leaves, there are @(2n-3)@ rooted trees. The root node
-- is moved. Branch lengths and branch support are merged according to the
-- 'Measurable' and 'BranchSupported' instances.
--
-- An error is thrown if the tree is not 'bifurcating'.
roots :: Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
roots (Node c []) = [Node c []]
roots t@(Node _ [Node _ [], Node _ []]) = [t]
roots t@(Node _ [_, _]) = t : lefts t ++ rights t
roots _ = error "roots: Tree is not bifurcating."

-- Move the root to the left.
lefts :: Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
lefts (Node c [Node l [Node x xs, Node y ys], Node r rs])
  | getBranchSupport l /= getBranchSupport r = error "lefts: Unequal branch support."
  | otherwise =
    let -- Left.
        x' = halve x
        l' = setBranchSupport (getBranchSupport x') $ setLen (getLen x') l
        r' = setLen (getLen r + getLen l) r
        tl = Node c [Node x' xs, Node l' [Node y ys, Node r' rs]]
        -- Right.
        y' = halve y
        l'' = setBranchSupport (getBranchSupport y') $ setLen (getLen y') l
        tr = Node c [Node l'' [Node r' rs, Node x xs], Node y' ys]
     in tl : tr : lefts tl ++ rights tr
lefts (Node _ [Node _ [], _]) = []
lefts (Node _ []) = error "lefts: THIS ERROR SHOULD NEVER HAPPEN. Encountered a leaf."
lefts _ = error "lefts: Tree is not bifurcating."

-- Move the root to the right.
rights :: Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
rights (Node c [Node l ls, Node r [Node x xs, Node y ys]])
  | getBranchSupport l /= getBranchSupport r = error "rights: Unequal branch support."
  | otherwise =
    let -- Left.
        x' = halve x
        r' = setBranchSupport (getBranchSupport x') $ setLen (getLen x') r
        l' = setLen (getLen r + getLen l) l
        tl = Node c [Node x' xs, Node r' [Node y ys, Node l' ls]]
        -- Right.
        y' = halve y
        r'' = setBranchSupport (getBranchSupport y') $ setLen (getLen y') r
        tr = Node c [Node r'' [Node l' ls, Node x xs], Node y' ys]
     in tl : tr : lefts tl ++ rights tr
rights (Node _ [_, Node _ []]) = []
rights (Node _ []) = error "rights: THIS ERROR SHOULD NEVER HAPPEN. Encountered a leaf."
rights _ = error "rights: Tree is not bifurcating."

-- | Root a bifurcating tree at a given point.
--
-- Root the tree at the midpoint of the branch defined by the given bipartition.
-- The original root node is moved to the new position.
--
-- - The tree has to be bifurcating (may be relaxed in the future).
-- - The leaves of the tree have to be unique.
-- - The leaves in the bipartition have to match the leaves of the tree.
rootAt :: Ord a => Bipartition (PhyloLabel a) -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
rootAt b t
  -- Tree is checked for being bifurcating in 'roots'.
  | length ls /= S.size lS = error "rootAt: Leaves of tree are not unique."
  | bS /= lS = error "rootAt: Bipartition does not match leaves of tree."
  | otherwise =
    fromMaybe
      (error "rootAt: Bipartition not found on tree.")
      (rootAt' b t)
  where
    bS = toSet b
    ls = S.fromList $ leaves t
    lS = S.fromList $ leaves t

-- Assume the leaves of the tree are unique.
rootAt' :: (Eq a, Ord a) => Bipartition (PhyloLabel a) -> Tree (PhyloLabel a) -> Maybe (Tree (PhyloLabel a))
rootAt' b t = find (\x -> b == bipartition x) (roots t)

-- | Connect two trees with a branch in all possible ways.
--
-- Basically, introduce a branch between two trees. If the trees have n, and m
-- branches, respectively, there are n*m ways to connect them.
--
-- A base node has to be given which will be used wherever the new node is
-- introduced.
connect :: PhyloLabel a -> Tree (PhyloLabel a) -> Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
connect n l r = [Node n [x, y] | x <- roots l, y <- roots r]

-- | Remove multifurcations by copying multifurcating nodes and introducing
-- branches with length 0 and branch support 0.
removeMultifurcations :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
removeMultifurcations t@(Node _ []) = t
removeMultifurcations (Node l [x]) = Node l [removeMultifurcations x]
removeMultifurcations (Node l [x, y]) = Node l $ map removeMultifurcations [x, y]
removeMultifurcations (Node l (x : xs)) = Node l $ map removeMultifurcations [x, Node l' xs]
  where
    l' = l {brLen = 0, brSup = 0}

-- | Add branch lengths. Set branch support to the lower support value. Forget
-- the parent node label.
extend :: PhyloLabel a -> PhyloLabel a -> PhyloLabel a
extend da pa = da {brSup = min (brSup pa) (brSup da), brLen = brLen pa + brLen da}

-- | Prune degree 2 nodes. Use 'extend' and 'pruneWith'.
prune :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
prune = pruneWith extend
