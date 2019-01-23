{- |
Module      :  EvoMod.Data.Tree.PhyloTree
Description :  Phylogenetic trees.
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 16:08:54 2019.

Phylogenetic nodes have a branch length and a label.

The easiest label type is 'Int': 'PhyloIntNode'.

Also, the 'Text' label is needed often: 'PhyloTextNode'.

-}


module EvoMod.Data.Tree.PhyloTree
  ( PhyloLabel (..)
  , PhyloIntNode
  , PhyloTextNode
  , PhyloTree
  , PhyloIntTree
  , PhyloTextTree
  ) where

import           Data.Text
import           Data.Tree
import           EvoMod.Data.Tree.MeasurableTree

-- | A primitive label type for phylogenetic trees with an 'Int' label and a
-- 'Double' branch length.
data PhyloLabel a = PhyloLabel { pLabel        :: a
                               , pBranchLength :: Double }
                 deriving (Show, Eq)

instance MeasurableLabel (PhyloLabel a) where
  branchLength = pBranchLength

-- | Tree node with 'Int' label.
type PhyloIntNode = PhyloLabel Int

-- | Tree node with 'Text' label. Important for parsing
-- 'EvoMod.Import.Tree.Newick' files.
type PhyloTextNode = PhyloLabel Text

-- | A phylogenetic tree with 'Double' branch lengths arbitrary node labels.
type PhyloTree a = Tree (PhyloLabel a)

-- | A phylogenetic tree with 'Double' branch lengths and 'Int' node labels.
type PhyloIntTree = Tree PhyloIntNode

-- | Phylogenetic tree with 'Double' branch lengths and 'Text' node labels.
type PhyloTextTree = Tree PhyloTextNode


