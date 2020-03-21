{- |
Module      :  ELynx.Data.Tree
Description :  Phylogenetic trees
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sat Mar 21 16:27:20 2020.

-}

module ELynx.Data.Tree
  ( module ELynx.Data.Tree.Tree
  , module ELynx.Data.Tree.PhyloTree
  , module ELynx.Data.Tree.Bipartition
  , module ELynx.Data.Tree.Multipartition
  , module ELynx.Data.Tree.Distance
  , module ELynx.Data.Tree.BranchSupportTree
  , module ELynx.Data.Tree.MeasurableTree
  , module ELynx.Data.Tree.NamedTree
  , module ELynx.Data.Tree.SubSample
  , module ELynx.Data.Tree.SumStat
  )
where

import           ELynx.Data.Tree.Bipartition
import           ELynx.Data.Tree.BranchSupportTree
import           ELynx.Data.Tree.Distance
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.Multipartition
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Data.Tree.SubSample
import           ELynx.Data.Tree.SumStat
import           ELynx.Data.Tree.Tree

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
