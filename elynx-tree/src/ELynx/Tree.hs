-- |
-- Module      :  ELynx.Tree
-- Description :  Rooted trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Mar 21 16:27:20 2020.
--
-- Convenience module combining all tree modules.
module ELynx.Tree
  ( -- * Rooted trees
    module ELynx.Tree.Rooted,

    -- * Tree zipper
    module ELynx.Tree.Zipper,

    -- * Parallel evaluation
    module ELynx.Tree.Parallel,

    -- * Branch label classes
    module ELynx.Tree.Measurable,
    module ELynx.Tree.Splittable,
    module ELynx.Tree.Supported,

    -- * Node label classes
    module ELynx.Tree.Named,

    -- * Phylogenies
    module ELynx.Tree.Phylogeny,

    -- * Partitions and distances
    module ELynx.Tree.Bipartition,
    module ELynx.Tree.Partition,
    module ELynx.Tree.Distance,

    -- * Import and Export
    module ELynx.Tree.Export.Newick,
    module ELynx.Tree.Export.Nexus,
    module ELynx.Tree.Import.Newick,
    module ELynx.Tree.Import.Nexus,
  )
where

import ELynx.Tree.Bipartition
import ELynx.Tree.Distance
import ELynx.Tree.Export.Newick
import ELynx.Tree.Export.Nexus
import ELynx.Tree.Import.Newick
import ELynx.Tree.Import.Nexus
import ELynx.Tree.Measurable
import ELynx.Tree.Named
import ELynx.Tree.Parallel
import ELynx.Tree.Partition
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import ELynx.Tree.Supported
import ELynx.Tree.Zipper

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
