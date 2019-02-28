{-# LANGUAGE FlexibleInstances #-}
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

The easiest label type is 'Int': 'PhyloIntLabel'.

Also, the 'ByteString' label is needed often: 'PhyloByteStringLabel'.

-}


module EvoMod.Data.Tree.PhyloTree
  ( PhyloIntLabel (..)
  , PhyloByteStringLabel (..)
  ) where

import qualified Data.ByteString.Lazy.Builder    as L
import qualified Data.ByteString.Lazy.Char8      as L
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.NamedTree

-- -- | A primitive label type for phylogenetic trees with an 'Int' label and a
-- -- 'Double' branch length.
-- data PhyloLabel a = PhyloLabel { pLabel :: a
--                                , pBrLen :: Double }
--                  deriving (Show, Eq)

-- | Tree node with 'Int' label.
data PhyloIntLabel = PhyloIntLabel { piLabel :: Int
                                   , piBrLen :: Double }
  deriving (Show, Eq)

--
instance Named PhyloIntLabel where
  name = L.toLazyByteString . L.intDec . piLabel

instance Measurable PhyloIntLabel where
  measure = piBrLen

-- | Tree node with 'ByteString' label. Important for parsing
-- 'EvoMod.Import.Tree.Newick' files.
data PhyloByteStringLabel = PhyloByteStringLabel { pbsLabel :: L.ByteString
                                                 , pbsBrLen :: Double }
  deriving (Show, Eq)

instance Named PhyloByteStringLabel where
  name = pbsLabel

instance Measurable PhyloByteStringLabel where
  measure = pbsBrLen

-- -- | A phylogenetic tree with 'Double' branch lengths arbitrary node labels.
-- type PhyloTree a = Tree (PhyloLabel a)

-- -- | A phylogenetic tree with 'Double' branch lengths and 'Int' node labels.
-- type PhyloIntTree = Tree PhyloIntLabel

-- -- | Phylogenetic tree with 'Double' branch lengths and 'ByteString' node labels.
-- type PhyloByteStringTree = Tree PhyloByteStringLabel

