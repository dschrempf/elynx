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
  ( PhyloNode (..)
  , PhyloIntNode
  , PhyloTextNode
  , PhyloTree
  , PhyloIntTree
  , PhyloTextTree
  ) where

import           Data.Text
import           Data.Tree
import           EvoMod.Data.Tree.MeasurableTree

-- | A primitive phylogenetic node with an 'Int' label and a 'Double' branch
-- length.
data PhyloNode a = PhyloNode { nLabel        :: a
                             , nBranchLength :: Double }

instance MeasurableNode (PhyloNode a) where
  branchLength = nBranchLength

-- | Tree node with 'Int' label.
type PhyloIntNode = PhyloNode Int

-- | Tree node with 'Text' label. Important for parsing
-- 'EvoMod.Import.Tree.Newick' files.
type PhyloTextNode = PhyloNode Text

-- | A phylogenetic tree with 'Double' branch lengths arbitrary node labels.
type PhyloTree a = Tree (PhyloNode a)

-- | A phylogenetic tree with 'Double' branch lengths and 'Int' node labels.
type PhyloIntTree = Tree PhyloIntNode

-- | Phylogenetic tree with 'Double' branch lengths and 'Text' node labels.
type PhyloTextTree = Tree PhyloTextNode


