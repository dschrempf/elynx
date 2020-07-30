-- TODO: Topology data type.
-- data Topology a = Node (NonEmptySet (Topology a)) | Leaf a

-- |
-- Module      :  ELynx.Data.Tree
-- Description :  Phylogenetic trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Mar 21 16:27:20 2020.
module ELynx.Data.Tree
  ( -- * Rooted trees
    module ELynx.Data.Tree.Rooted,
    module ELynx.Data.Tree.Zipper,

    -- * Branch label classes
    module ELynx.Data.Tree.Measurable,
    module ELynx.Data.Tree.Splittable,
    module ELynx.Data.Tree.Supported,

    -- * Node label classes
    module ELynx.Data.Tree.Named,

    -- * Phylogenies
    module ELynx.Data.Tree.Phylogeny,

    -- * Partitions and distances
    module ELynx.Data.Tree.Bipartition,
    module ELynx.Data.Tree.Partition,
    module ELynx.Data.Tree.Distance,
  )
where

import ELynx.Data.Tree.Bipartition
import ELynx.Data.Tree.Distance
import ELynx.Data.Tree.Measurable
import ELynx.Data.Tree.Named
import ELynx.Data.Tree.Partition
import ELynx.Data.Tree.Phylogeny
import ELynx.Data.Tree.Rooted
import ELynx.Data.Tree.Splittable
import ELynx.Data.Tree.Supported
import ELynx.Data.Tree.Zipper

-- -- | An evolutionary label has some information about where the corresponding
-- -- node is on the tree, and if the node is 'extant', 'extinct', 'internal', or
-- -- 'external'. The latter two could also be determined from the tree. This could
-- -- be species, genes or individuals; probably more.
-- class EvoLabel n where
--   extant          :: n -> Bool
--   extinct         :: n -> Bool

--   internal        :: n -> Bool
--   internal n = not $ extant n || extinct n

--   external        :: n -> Bool
--   external   = not . internal

-- -- -- | Glue branches together, so that one new tree emerges. It's root node is
-- -- -- new, the sub-forest has to be given (a list of trees).
-- -- glue :: (NodeType c)
-- --      => PhyloLabel a b c       -- ^ New root node.
-- --      -> [PhyloTree a b c]      -- ^ Sub-forest.
-- --      -> PhyloTree a b c
-- -- glue s@(PhyloLabel _ _ n) ts
-- --   | extant n  = error "Root node cannot be of type 'Exant'."
-- --   | extinct n = error "Root node cannot be of type 'Extinct'."
-- --   | otherwise = Node s ts
