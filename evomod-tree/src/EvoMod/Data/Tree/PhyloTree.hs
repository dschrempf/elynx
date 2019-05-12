{-# LANGUAGE FlexibleInstances #-}

{- |
Module      :  EvoMod.Data.Tree.PhyloTree
Description :  Phylogenetic trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 16:08:54 2019.

Phylogenetic nodes have a branch length and a label.

The easiest label type is 'Int': 'PhyloIntLabel'.

Also, the 'L.ByteString' label is needed often: 'PhyloByteStringLabel'.

XXX: This is all too complicated. Maybe I should just define a standard tree object like
> data PhyloTree a = Tree (PhyloLabel a)
and that's it. Forget about type classes like Measurable, Named and so on.

-}


module EvoMod.Data.Tree.PhyloTree
  ( PhyloLabel (..)
  , PhyloIntLabel
  , PhyloByteStringLabel
  ) where

import qualified Data.ByteString.Lazy.Builder    as L
import qualified Data.ByteString.Lazy.Char8      as L
import           Data.Function

import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.NamedTree

-- | A primitive label type for phylogenetic trees with an 'Int' label and a
-- 'Double' branch length.
data PhyloLabel a = PhyloLabel { pLabel :: a
                               , pBrLen :: Double }
                 deriving (Read, Show, Eq)

instance Ord a => Ord (PhyloLabel a) where
  compare = compare `on` pLabel

instance Measurable (PhyloLabel a) where
  getLen = pBrLen
  setLen l (PhyloLabel lbl _) | l >= 0 = PhyloLabel lbl l
                           | otherwise = error "Branch lengths cannot be negative."

-- | Tree node with 'Int' label.
type PhyloIntLabel = PhyloLabel Int

instance Named PhyloIntLabel where
  name = L.toLazyByteString . L.intDec . pLabel

-- | Tree node with 'L.ByteString' label. Important for parsing
-- 'EvoMod.Import.Tree.Newick' files.
type PhyloByteStringLabel = PhyloLabel L.ByteString

instance Named PhyloByteStringLabel where
  name = pLabel

-- -- | A phylogenetic tree with 'Double' branch lengths arbitrary node labels.
-- type PhyloTree a = Tree (PhyloLabel a)

-- -- | A phylogenetic tree with 'Double' branch lengths and 'Int' node labels.
-- type PhyloIntTree = Tree PhyloIntLabel

-- -- | Phylogenetic tree with 'Double' branch lengths and 'ByteString' node labels.
-- type PhyloByteStringTree = Tree PhyloByteStringLabel

