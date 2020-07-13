-- |
-- Module      :  ELynx.Data.Tree.Phylogeny
-- Description :  Phylogenetic trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 16:08:54 2019.
--
-- A phylogeny is a 'Tree' with branch and node labels. The node labels are
-- unique, and the order of the trees in the sub-forest is meaningless.
--
-- Internally, however, the underlying 'Tree' data structure stores the
-- sub-forest as a list, which has a specific order. Hence, we have to do some
-- tricks when comparing trees, and tree comparison is slow.
--
-- Also, the uniqueness of the leaves is not ensured by the data type, but has
-- to be checked at runtime.
--
-- NOTE: Trees in this library are all rooted.
module ELynx.Data.Tree.Phylogeny
  ( equal,
    equalTopology,
    valid,
    -- PhyloLabel (..),
    -- Length (..),
    -- toLength,
    -- fromLength,
    -- Support (..),
    -- toSupport,
  )
where

import Data.Bifunctor
-- import Data.Maybe (fromMaybe)
import qualified Data.Set as S
-- import ELynx.Data.Tree.Measurable
-- import ELynx.Data.Tree.Named
-- import ELynx.Data.Tree.Supported
import ELynx.Data.Tree.Rooted

-- | The equality check is slow because the order of children is arbitrary.
equal :: (Eq e, Eq a) => Tree e a -> Tree e a -> Bool
equal ~(Node brL lbL tsL) ~(Node brR lbR tsR) =
  (brL == brR)
    && (lbL == lbR)
    && (length tsL == length tsR)
    && all (`elem` tsR) tsL

-- | Check if two trees have the same topology.
equalTopology :: Eq a => Tree e a -> Tree e a -> Bool
equalTopology l r = rmBr l == rmBr r
  where
    rmBr = first (const ())

hasNoDuplicates :: Ord a => [a] -> Bool
hasNoDuplicates = go S.empty
  where
    go _ [] = True
    go seen (x : xs) = x `S.notMember` seen && go (S.insert x seen) xs

-- | Check if a tree is valid, that is, if the leaves are unique.
valid :: Ord a => Tree e a -> Bool
valid = hasNoDuplicates . leaves

-- -- TODO: Rename to Phylo.

-- -- | A label type for phylogenetic trees possibly with branch length and branch support.
-- data PhyloLabel a = PhyloLabel
--   { pLabel :: a,
--     pBrLen :: Maybe BranchLength,
--     pBrSup :: Maybe BranchSupport
--   }
--   deriving (Read, Show, Eq, Ord)

-- instance Named a => Named (PhyloLabel a) where
--   getName = getName . pLabel

-- -- | A label with a branch length. For conversion, see 'toLength' and
-- -- 'fromLength'.
-- data Length a = Length
--   { lLabel :: a,
--     brLen :: BranchLength
--   }
--   deriving (Read, Show, Eq, Ord)

-- instance Named a => Named (Length a) where
--   getName = getName . lLabel

-- instance Measurable (Length a) where
--   getLen = brLen
--   setLen x l = l {brLen = x}

-- -- | If root branch length is not available, set it to 0. Return 'Nothing' if
-- -- any other branch length is unavailable.
-- toLength :: Tree (PhyloLabel a) -> Maybe (Tree (Length a))
-- toLength = traverse toLengthLabel . cleanRootLength

-- cleanRootLength :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- cleanRootLength (Node (PhyloLabel l Nothing s) f) = Node (PhyloLabel l (Just 0) s) f
-- cleanRootLength _ = id

-- toLengthLabel :: PhyloLabel a -> Maybe (Length a)
-- toLengthLabel (PhyloLabel l Nothing _) = Nothing
-- toLengthLabel (PhyloLabel l (Just b) _) = Length l b

-- -- | Set all branch support values to 'Nothing'. Useful, for example, to export
-- -- a tree with branch lengths in Newick format.
-- fromLength :: Tree (Length a) -> Tree (PhyloLabel a)
-- fromLength = fmap fromLengthLabel

-- fromLengthLabel :: Length a -> PhyloLabel a
-- fromLengthLabel (Length l b) = PhyloLabel l (Just b) Nothing

-- -- | A label with branch support. For conversion, see 'toSupport'.
-- data Support a = Support
--   { sLabel :: a,
--     brSup :: BranchSupport
--   }
--   deriving (Read, Show, Eq, Ord)

-- instance Named a => Named (Support a) where
--   getName = getName . spLabel

-- instance Supported (Support a) where
--   getBranchSupport = brSup
--   setBranchSupport x l = l {brSup = x}

-- getMaxBrSup :: Tree (PhyloLabel a) -> BranchSupport
-- getMaxBrSup =
--   -- If all branch support values are below 1.0, set the max support to 1.0.
--   max 1.0
--     .
--     -- If no branch support is given, set max support to 1.0.
--     fromMaybe 1.0
--     . maximum
--     . fmap pBrSup

-- -- | Set branch support values of branches leading to the leaves to maximum
-- -- support. Set the root branch support to maximum support.
-- --
-- -- Return 'Nothing' if any other branch has no available support value.
-- toSupport :: Tree (PhyloLabel a) -> Maybe (Tree (Support a))
-- toSupport t = traverse toSupportLabel . cleanLeafSupport m . cleanRootSupport m
--   where
--     m = getMaxBrSup t

-- cleanRootSupport :: BranchSupport -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- cleanRootSupport maxSup (Node (PhyloLabel l b Nothing) xs) = Node (PhyloLabel l (Just maxSup)) xs
-- cleanRootSupport _ t = t

-- cleanLeafSupport :: BranchSupport -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- cleanLeafSupport maxSup (Node (PhyloLabel l b Nothing) []) = Node (PhyloLabel l (Just maxSup)) []
-- cleanLeafSupport _ (Node l xs) = Node l $ map cleanLeafSupport xs

-- toSupportLabel :: PhyloLabel a -> Maybe (Support a)
-- toSupportLabel (PhyloLabel l _ Nothing) = Nothing
-- toSupportLabel (PhyloLabel l _ (Just s)) = Support l s
