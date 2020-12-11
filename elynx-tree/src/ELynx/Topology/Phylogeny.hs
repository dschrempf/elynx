-- |
-- Module      :  ELynx.Topology.Phylogeny
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
-- THIS MODULE IS INCOMPLETE.
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
-- Note: 'Topology's are rooted.
--
-- Note: 'Topology's encoded in Newick format correspond to rooted topologies.
-- By convention only, a topology parsed from Newick format is usually thought
-- to be unrooted, when the root node is multifurcating and has three or more
-- children. This convention is not enforced here. Newick topologies are just
-- parsed as they are, and a rooted topology is returned.
--
-- THIS MODULE IS INCOMPLETE.
module ELynx.Topology.Phylogeny
  ( equal,
  )
where

import Data.List
import Data.Maybe
import ELynx.Topology.Rooted

-- | The equality check is slow because the order of children is considered to
-- be arbitrary.
--
-- NOTE: The equality check is only meaningful if the topologies have unique
-- leaves.
--
-- Return 'Left' if a topology does not have unique leaves.
equal :: Eq a => Topology a -> Topology a -> Bool
equal (Node tsL) (Node tsR) =
  (length tsL == length tsR)
    && all (\t -> isJust $ find (equal t) tsR) tsL
equal (Leaf lbL) (Leaf lbR) = lbL == lbR
equal _ _ = False

-- TODO.

-- A multifurcating root node can be resolved to a bifurcating root node with
-- 'outgroup'.
--
-- The bifurcating root node can be changed with 'outgroup' or 'midpoint'.
--
-- For a given topology with bifurcating root node, a list of all rooted
-- topologies is returned by 'roots'.
