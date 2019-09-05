{- |
Module      :  EvoMod.Data.Tree.EvoTree
Description :  Evolutionary nodes
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 14:19:26 2019.

XXX: This module is not used.

-}

module EvoMod.Data.Tree.EvoTree
  ( EvoLabel (..)
  ) where

-- | An evolutionary label has some information about where the corresponding
-- node is on the tree, and if the node is 'extant', 'extinct', 'internal', or
-- 'external'. The latter two could also be determined from the tree. This could
-- be species, genes or individuals; probably more.
class EvoLabel n where
  extant          :: n -> Bool
  extinct         :: n -> Bool

  internal        :: n -> Bool
  internal n = not $ extant n || extinct n
  external        :: n -> Bool
  external   = not . internal

-- -- | Glue branches together, so that one new tree emerges. It's root node is
-- -- new, the sub-forest has to be given (a list of trees).
-- glue :: (NodeType c)
--      => PhyloLabel a b c       -- ^ New root node.
--      -> [PhyloTree a b c]      -- ^ Sub-forest.
--      -> PhyloTree a b c
-- glue s@(PhyloLabel _ _ n) ts
--   | extant n  = error "Root node cannot be of type 'Exant'."
--   | extinct n = error "Root node cannot be of type 'Extinct'."
--   | otherwise = Node s ts
