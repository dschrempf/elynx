{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

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
  ( -- * Functions
    equal,
    equalTopology,
    intersect,
    bifurcating,
    resolve,
    roots,
    rootAt,
    connect,

    -- * Branch labels
    Phylo (..),
    Length (..),
    phyloToLengthTree,
    lengthToPhyloTree,
    Support (..),
    phyloToSupportTree,
    phyloToSupportTreeUnsafe,
    PhyloStrict (..),
    toStrictTree,
    fromStrictTree,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.List hiding (intersect)
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import ELynx.Data.Tree.Bipartition
import ELynx.Data.Tree.Measurable
import ELynx.Data.Tree.Rooted
import ELynx.Data.Tree.Supported

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

-- | Compute the intersection of trees.
--
-- The intersections are the largest subtrees sharing the same leaf set.
--
-- Degree two nodes are pruned with 'prune'.
--
-- Return 'Left' if:
-- - any tree has duplicate leaves;
-- - the intersection of leaves is empty.
intersect ::
  (Semigroup e, Eq e, Ord a) => Forest e a -> Either String (Forest e a)
intersect ts
  -- any (map (not . valid) ts) = Left "intersect: A tree is invalid."
  | S.null lvsCommon = Left "intersect: Intersection of leaves is empty."
  | otherwise = case sequence [dropLeavesWith (predicate ls) t | (ls, t) <- zip leavesToDrop ts] of
    Nothing -> Left "intersect: A tree is empty."
    Just ts' -> Right ts'
  where
    -- Leaf sets.
    lvss = map (S.fromList . leaves) ts
    -- Common leaf set.
    lvsCommon = foldl1' S.intersection lvss
    -- Leaves to drop for each tree in the forest.
    leavesToDrop = map (S.\\ lvsCommon) lvss
    -- Predicate.
    predicate lvsToDr l = l `S.member` lvsToDr

-- | Check if a tree is bifurcating.
--
-- A Bifurcating tree only contains degree one and degree three nodes.
bifurcating :: Tree e a -> Bool
bifurcating (Node _ _ []) = True
bifurcating (Node _ _ [x, y]) = bifurcating x && bifurcating y
bifurcating _ = False

-- | Remove multifurcations.
--
-- A caterpillar like bifurcating tree is used to resolve multifurcations. The
-- multifurcating nodes are copied.
--
-- Branch labels are not handled.
resolve :: Tree () a -> Tree () a
resolve t@(Node _ _ []) = t
resolve (Node _ l [x]) = Node () l [resolve x]
resolve (Node _ l [x, y]) = Node () l $ map resolve [x, y]
resolve (Node _ l (x : xs)) = Node () l $ map resolve [x, Node () l xs]

-- | For a rooted, bifurcating tree, get all possible rooted (bifurcating) trees.
--
-- For a tree with @n>2@ leaves, there are @(2n-3)@ rooted trees. The root node
-- is moved. See also 'ELynx.Data.Tree.Bipartition.rootAt'.
--
-- Branch labels are not handled.
--
-- Return 'Left' if the tree is not 'bifurcating'.
roots :: Tree () a -> Either String (Forest () a)
roots t@(Node _ _ []) = Right [t]
roots t@(Node _ _ [Node _ _ [], Node _ _ []]) = Right [t]
roots t@(Node _ _ [_, _]) = sequence $ Right t : sequence (lefts t) ++ sequence (rights t)
roots _ = Left "roots: Tree is not bifurcating."

-- Move the root to the left.
lefts :: Tree () a -> Either String (Forest () a)
lefts (Node _ c [Node _ l [Node _ x tsX, Node _ y tsY], Node _ r tsR]) =
  let -- Left.
      tl = Node () c [Node () x tsX, Node () l [Node () y tsY, Node () r tsR]]
      -- Right.
      tr = Node () c [Node () l [Node () r tsR, Node () x tsX], Node () y tsY]
   in sequence $ Right tl : Right tr : sequence (lefts tl) ++ sequence (rights tr)
lefts (Node _ _ [Node _ _ [], _]) = Right []
lefts (Node _ _ []) = Left "lefts: This is a bug. Encountered a leaf."
lefts _ = Left "lefts: Tree is not bifurcating."

-- Move the root to the right.
rights :: Tree () a -> Either String (Forest () a)
rights (Node _ c [Node _ l tsL, Node _ r [Node _ x tsX, Node _ y tsY]]) =
  let -- Left.
      tl = Node () c [Node () x tsX, Node () r [Node () y tsY, Node () l tsL]]
      -- Right.
      tr = Node () c [Node () r [Node () l tsL, Node () x tsX], Node () y tsY]
   in sequence $ Right tl : Right tr : sequence (lefts tl) ++ sequence (rights tr)
rights (Node _ _ [_, Node _ _ []]) = Right []
rights (Node _ _ []) = Left "rights: This is a bug. Encountered a leaf."
rights _ = Left "rights: Tree is not bifurcating."

-- | Root a tree.
--
-- Root the tree at the branch defined by the given bipartition. The original
-- root node is moved to the new position. See also
-- 'ELynx.Data.Tree.Rooted.roots'.
--
-- Branch labels are not handled.
--
-- Return 'Left', if:
-- - the tree is not bifurcating;
-- - the tree has duplicate leaves;
-- - the bipartition does not match the leaves of the tree.
rootAt :: Ord a => Bipartition a -> Tree () a -> Either String (Tree () a)
rootAt b t
  -- Tree is checked for being bifurcating in 'roots'.
  -- Do not use 'valid' here, because we also need to compare the leaf set with the bipartition.
  | length lvLst /= S.size lvSet = Left "rootAt: Leaves of tree are not unique."
  | toSet b /= lvSet = Left "rootAt: Bipartition does not match leaves of tree."
  | otherwise = rootAt' b t
  where
    lvLst = leaves t
    lvSet = S.fromList $ leaves t

-- Assume the leaves of the tree are unique.
rootAt' :: (Eq a, Ord a) => Bipartition a -> Tree () a -> Either String (Tree () a)
rootAt' b t = do
  ts <- roots t
  case find (\x -> Right b == bipartition x) ts of
    Nothing -> Left "rootAt': Bipartition not found on tree."
    Just t' -> Right t'

-- | Connect two trees with a branch in all possible ways.
--
-- Introduce a branch between two trees. If the trees have n, and m branches,
-- respectively, there are n*m ways to connect them.
--
-- A base node label has to be given which will be used wherever the new node is
-- introduced.
--
-- Branch labels are not handled.
connect :: a -> Tree () a -> Tree () a -> Either String (Forest () a)
connect lb l r = do
  ls <- roots l
  rs <- roots r
  return [Node () lb [x, y] | x <- ls, y <- rs]

-- | Branch label for phylogenetic trees.
--
-- Branches may have a length and a support value.
data Phylo = Phylo
  { brLen :: Maybe BranchLength,
    brSup :: Maybe BranchSupport
  }
  deriving (Read, Show, Eq, Ord)

instance Semigroup Phylo where
  Phylo mBL mSL <> Phylo mBR mSR =
    Phylo
      (getSum <$> (Sum <$> mBL) <> (Sum <$> mBR))
      (getMin <$> (Min <$> mSL) <> (Min <$> mSR))

-- | Branch length label.
--
-- For conversion, see 'phyloToLengthTree' and 'lengthToPhyloTree'.
newtype Length = Length {fromLength :: BranchLength}
  deriving (Read, Show, Eq, Ord, Num, Fractional, Floating)
  deriving (Semigroup, Monoid) via Sum Double

instance Measurable Length where
  getLen = fromLength
  setLen b _ = Length b

-- | If root branch length is not available, set it to 0.
--
-- Return 'Left' if any other branch length is unavailable.
phyloToLengthTree :: Tree Phylo a -> Either String (Tree Length a)
phyloToLengthTree =
  maybe (Left "phyloToLengthTree: Length unavailable for some branches.") Right
    . bitraverse toLength pure
    . cleanRootLength

cleanRootLength :: Tree Phylo a -> Tree Phylo a
cleanRootLength (Node (Phylo Nothing s) l f) = Node (Phylo (Just 0) s) l f
cleanRootLength t = t

toLength :: Phylo -> Maybe Length
toLength p = Length <$> brLen p

-- | Set all branch support values to 'Nothing'.
--
-- Useful, for example, to export a tree with branch lengths in Newick format.
lengthToPhyloTree :: Tree Length a -> Tree Phylo a
lengthToPhyloTree = first fromLengthLabel

fromLengthLabel :: Length -> Phylo
fromLengthLabel (Length b) = Phylo (Just b) Nothing

-- | Branch support label.
--
-- For conversion, see 'phyloToSupportTree'.
newtype Support = Support {fromSupport :: BranchSupport}
  deriving (Read, Show, Eq, Ord, Num, Fractional, Floating)
  deriving (Semigroup) via Min Double

instance Supported Support where
  getSup = fromSupport
  setSup s _ = Support s

-- | Set branch support values of branches leading to the leaves and of the root
-- branch to maximum support.
--
-- Return 'Left' if any other branch has no available support value.
phyloToSupportTree :: Tree Phylo a -> Either String (Tree Support a)
phyloToSupportTree t =
  maybe
    (Left "phyloToSupportTree: Support unavailable for some branches.")
    Right
    $ bitraverse toSupport pure $ cleanLeafSupport m $ cleanRootSupport m t
  where
    m = getMaxSupport t

-- | Set all unavailable branch support values to maximum support.
phyloToSupportTreeUnsafe :: Tree Phylo a -> Tree Support a
phyloToSupportTreeUnsafe t = cleanSupport m t
  where
    m = getMaxSupport t

-- If all branch support values are below 1.0, set the max support to 1.0.
getMaxSupport :: Tree Phylo a -> BranchSupport
getMaxSupport = fromJust . max (Just 1.0) . bimaximum . bimap brSup (const Nothing)

cleanRootSupport :: BranchSupport -> Tree Phylo a -> Tree Phylo a
cleanRootSupport maxSup (Node (Phylo b Nothing) l xs) = Node (Phylo b (Just maxSup)) l xs
cleanRootSupport _ t = t

cleanLeafSupport :: BranchSupport -> Tree Phylo a -> Tree Phylo a
cleanLeafSupport s (Node (Phylo b Nothing) l []) = Node (Phylo b (Just s)) l []
cleanLeafSupport s (Node b l xs) = Node b l $ map (cleanLeafSupport s) xs

toSupport :: Phylo -> Maybe Support
toSupport (Phylo _ Nothing) = Nothing
toSupport (Phylo _ (Just s)) = Just $ Support s

cleanSupport :: BranchSupport -> Tree Phylo a -> Tree Support a
cleanSupport maxSup (Node (Phylo _ s) l xs) = Node (Support $ fromMaybe maxSup s) l $ map (cleanSupport maxSup) xs

-- | Strict branch label for phylogenetic trees.
data PhyloStrict = PhyloStrict
  { sBrLen :: BranchLength,
    sBrSup :: BranchSupport
  }
  deriving (Read, Show, Eq, Ord)

instance Semigroup PhyloStrict where
  PhyloStrict bL sL <> PhyloStrict bR sR = PhyloStrict (bL + bR) (min sL sR)

instance Measurable PhyloStrict where
  getLen = sBrLen
  setLen b l = l {sBrLen = b}

instance Supported PhyloStrict where
  getSup = sBrSup
  setSup s l = l {sBrSup = s}

-- | Conversion to a 'PhyloStrict' tree.
--
-- See 'phyloToLengthTree' and 'phyloToSupportTree'.
toStrictTree :: Tree Phylo a -> Either String (Tree PhyloStrict a)
toStrictTree t = do
  lt <- first fromLength <$> phyloToLengthTree t
  st <- first fromSupport <$> phyloToSupportTree t
  case zipTreesWith PhyloStrict const lt st of
    Nothing -> error "toStrictTree: This is a bug. Can not zip two trees with the same topology."
    Just zt -> return zt

-- | Set all branch length and support values to 'Just' the value.
--
-- Useful, for example, to export a tree with branch lengths in Newick format.
fromStrictTree :: Tree PhyloStrict a -> Tree Phylo a
fromStrictTree = first fromStrictLabel

fromStrictLabel :: PhyloStrict -> Phylo
fromStrictLabel (PhyloStrict b s) = Phylo (Just b) (Just s)
