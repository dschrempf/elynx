-- |
-- Module      :  ELynx.Data.Topology.Phylogeny
-- Description :  Phylogenetic topologies
-- Copyright   :  (c) Dominik Schrempf, 2020
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
-- Internally, however, the underlying 'Topology' data structure stores the
-- sub-forest as a (non-empty) list, which has a specific order. Hence, we have
-- to do some tricks when comparing topologies, and topology comparison is slow.
--
-- Also, the uniqueness of the leaves is not ensured by the data type, but has
-- to be checked at runtime. Functions relying on the tree to have unique leaves
-- do perform this check, and return 'Left' with an error message, if the tree
-- has duplicate leaves.
--
-- Note: Topologies are rooted.
--
-- Note: Topologies encoded in Newick format correspond to rooted topologies. By
-- convention only, a topology parsed from Newick format is usually thought to
-- be unrooted, when the root node is multifurcating and has three children.
-- This convention is not enforced here. Newick topologies are just parsed as
-- they are, and a rooted topology is returned.
--
-- The bifurcating root of a topology can be changed with 'roots', or 'rootAt'.
--
-- Topologies with multifurcating root nodes can be properly rooted using
-- 'outgroup'.
module ELynx.Data.Topology.Phylogeny
  ( outgroup,
    roots,
    rootAt,
  )
where

import Data.Set (Set)
import ELynx.Data.Topology.Rooted
import ELynx.Data.Tree.Bipartition

-- TODO.

-- -- | Remove multifurcations.
-- --
-- -- A caterpillar like bifurcating tree is used to resolve all multifurcations on
-- -- a tree. The multifurcating nodes are copied.
-- --
-- -- Branch labels are not handled.
-- resolve :: Tree () a -> Tree () a
-- resolve t@(Node _ _ []) = t
-- resolve (Node _ l [x]) = Node () l [resolve x]
-- resolve (Node _ l [x, y]) = Node () l $ map resolve [x, y]
-- resolve (Node _ l (x : xs)) = Node () l $ map resolve [x, Node () l xs]

-- | Resolve a multifurcation at the root using an outgroup.
outgroup :: Ord a => Set a -> Topology a -> Either String (Topology a)
outgroup = undefined

-- -- | For a rooted tree with a bifurcating root node, get all possible rooted
-- -- trees.
-- --
-- -- The root node is moved.
-- --
-- -- For a tree with @l=2@ leaves, there is one rooted tree. For a bifurcating
-- -- tree with @l>2@ leaves, there are @(2l-3)@ rooted trees. For a general tree
-- -- with a bifurcating root node, and a total number of @n>2@ nodes, there are
-- -- (n-2) rooted trees.
-- --
-- -- Moving a multifurcating root node to another branch would change the
-- -- topology, and so, a bifurcating root is required. To resolve a multifurcating
-- -- root, please see and use TODO.
-- --
-- -- Branch labels are not handled, but see 'rootsBranch'.
-- --
-- -- 'rootAt' roots the tree at a specific position.
-- --
-- -- Return 'Left' if the root node is not 'bifurcating'.
-- roots :: Tree () a -> Either String (Forest () a)
-- roots (Node _ _ []) = Left "roots: Root node is a leaf."
-- roots (Node _ _ [_]) = Left "roots: Root node has degree two."
-- roots t@(Node _ c [tL, tR]) = Right $ t : descend id () c tR tL ++ descend id () c tL tR
-- roots _ = Left "roots: Root node is multifurcating."

-- | For a rooted topology with a bifurcating root node, get all possible rooted
-- topologies.
roots :: Topology a -> Either String (Forest a)
roots = undefined

-- -- | Root a tree at a specific position.
-- --
-- -- Root the tree at the branch defined by the given bipartition. The original
-- -- root node is moved to the new position.
-- --
-- -- The root node must be bifurcating (see 'roots').
-- --
-- -- Branch labels are not handled, but see 'rootAtBranch'.
-- --
-- -- Return 'Left', if:
-- -- - the root node is not bifurcating;
-- -- - the tree has duplicate leaves;
-- -- - the bipartition does not match the leaves of the tree.
-- rootAt :: Ord a => Bipartition a -> Tree () a -> Either String (Tree () a)
-- rootAt = rootAtBranch id

-- | Root a tree at a specific position.
rootAt :: Ord a => Bipartition a -> Topology a -> Either String (Forest a)
rootAt = undefined
