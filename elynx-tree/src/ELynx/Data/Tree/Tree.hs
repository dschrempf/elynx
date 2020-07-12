{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      :  ELynx.Data.Tree.Tree
-- Description :  Functions related to phylogenetic trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 09:57:29 2019.
--
-- Functions to work with rooted, rose 'Tree's with labeled branches. For
-- comparisons between trees, the order of children is meaningless. However, the
-- underlying data structure stores the children as a list, which has a specific
-- order.
--
-- Comment about nomenclature:
--
-- - A 'Tree' is defined as
--
-- @
-- data Tree e a = Node
--   { branch :: e,
--     label :: a,
--     children :: [Tree e a]
--   }
-- @
--
-- This means, that the word 'Node' is reserved for the constructor of a tree,
-- and that a 'Node' has an attached branch, a label, and children. The terms
-- 'Node' and /label/ are not to be confused.
--
-- Using the 'Tree' data type has some disadvantages:
--
-- 1. All trees are rooted. Unrooted trees can be treated with a rooted data
-- structure, as it is used here. However, some functions may be meaningless.
--
-- 2. Internally, the children are ordered, so we have to do some tricks when
-- comparing trees.
--
-- 3. Changing branch labels, node labels, or the topology of the tree are slow
-- operations, especially, when the changes are close to the leaves of the tree.
--
-- 4. The uniqueness of the leaves has to be checked at runtime and is not
-- ensured by the data type.
--
-- NOTE: Trees in this library are all rooted.
module ELynx.Data.Tree.Tree
  ( Tree (branch, label, children),
    singleton,
    valid,
    degree,
    leaves,
    pruneWith,
    dropLeafWith,
    intersectWith,
    -- merge,
    -- tZipWith,
    partitionTree,
    -- subForestGetSubsets,
    -- subTree,
    -- bifurcating,
    -- clades,
  )
where

import Control.Comonad
import Control.DeepSeq
-- import Control.Monad (zipWithM)
-- import Control.Monad.Zip
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Data
import Data.List
-- import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics

-- import Data.Traversable

-- | Rooted rose trees with branch labels.
--
-- NOTE: Trees in this library are all rooted.
--
-- NOTE: Unary instances such as 'Functor' act on node labels, and not on branch
-- labels. Binary instances such as 'Bifunctor' act on both labels.
data Tree e a = Node
  { branch :: e,
    label :: a,
    children :: Forest e a
  }
  deriving (Read, Show, Data, Generic, Generic1)

-- | A shorthand.
type Forest e a = [Tree e a]

-- This is expected to be very slow. Can we do better?
instance (Eq e, Eq a) => Eq (Tree e a) where
  ~(Node brL lbL tsL) == ~(Node brR lbR tsR) =
    (brL == brR)
      && (lbL == lbR)
      && (length tsL == length tsR)
      && all (`elem` tsR) tsL

instance Functor (Tree e) where
  fmap f ~(Node br lb ts) = Node br (f lb) $ map (fmap f) ts
  x <$ ~(Node br _ ts) = Node br x (map (x <$) ts)

instance Bifunctor Tree where
  bimap f g ~(Node br lb ts) = Node (f br) (g lb) $ map (bimap f g) ts
  first f ~(Node br lb ts) = Node (f br) lb $ map (first f) ts
  second g ~(Node br lb ts) = Node br (g lb) $ map (second g) ts

instance Foldable (Tree e) where
  foldMap f ~(Node _ lb ts) = f lb <> foldMap (foldMap f) ts

instance Bifoldable Tree where
  bifoldMap f g ~(Node br lb ts) = f br <> g lb <> foldMap (bifoldMap f g) ts

instance Traversable (Tree e) where
  traverse g ~(Node br lb ts) = Node br <$> g lb <*> traverse (traverse g) ts

instance Bitraversable Tree where
  bitraverse f g ~(Node br lb ts) = Node <$> f br <*> g lb <*> traverse (bitraverse f g) ts

-- -- XXX: I am unsure how to provide 'Applicative' and 'Monad' instances since
-- -- the branch label is undefined when using 'pure'.

-- instance MonadZip (Tree e) where
--   mzipWith g (Node brL lbL tsL) (Node _ lbR tsR) =
--     Node brL (g lbL lbR) (mzipWith (mzipWith g) tsL tsR)

--   munzip (Node br (lbL, lbR) ts) = (Node br lbL tsL, Node br lbR tsR)
--     where
--       (tsL, tsR) = munzip (map munzip ts)

instance Comonad (Tree e) where
  duplicate t@(Node br _ ts) = Node br t (map duplicate ts)
  extract (Node _ lb _) = lb
  {-# INLINE extract #-}

instance (NFData e, NFData a) => NFData (Tree e a) where
  rnf (Node br lb ts) = rnf br `seq` rnf lb `seq` rnf ts

-- TODO: ToJSON, FromJSON.

-- | The simplest tree, a leaf.
singleton :: e -> a -> Tree e a
singleton br lb = Node br lb []

hasNoDuplicates :: Ord a => [a] -> Bool
hasNoDuplicates = go S.empty
  where
    go _ [] = True
    go seen (x : xs) = x `S.notMember` seen && go (S.insert x seen) xs

-- | Check if a tree is valid, that is, if the leaves are unique.
valid :: Ord a => Tree e a -> Bool
valid = hasNoDuplicates . leaves

-- | The degree of the root node of a tree.
degree :: Tree e a -> Int
degree = (+ 1) . length . children

-- | Get leaves.
leaves :: Ord a => Tree e a -> [a]
leaves (Node _ lb []) = [lb]
leaves (Node _ _ xs) = concatMap leaves xs

-- | Prune degree two nodes.
--
-- The information stored in a pruned node is lost. The branches are combined
-- with a given function of the form @\daughterBranch parentBranch ->
-- combinedBranch@.
pruneWith :: (Eq e, Ord a) => (e -> e -> e) -> Tree e a -> Tree e a
pruneWith _ t@(Node _ _ []) = t
pruneWith f (Node paBr _ [Node daBr daLb daXs]) = Node (f daBr paBr) daLb daXs
pruneWith f (Node paBr paLb paXs) = Node paBr paLb $ map (pruneWith f) paXs

-- | Drop a leaf from a tree.
--
-- The possibly resulting degree two node is pruned and the branches are
-- combined using a given function (see 'pruneWith').
--
-- The same tree is returned, if the leaf is not found on the tree.
dropLeafWith :: (Eq e, Ord a) => (e -> e -> e) -> a -> Tree e a -> Tree e a
dropLeafWith f l (Node paBr paLb paXs) =
  case paXs' of
    [Node daBr daLb daXs] -> Node (f daBr paBr) daLb daXs
    _ -> Node paBr paLb paXs'
  where
    toRm x = null (children x) && label x == l
    paXs' = map (dropLeafWith f l) (filter (not . toRm) paXs)

-- | Compute the intersection of trees.
--
-- The intersections are the largest subtrees sharing the same leaf set. Leaf
-- are compared using a given function. Leaves are dropped with 'dropLeafWith',
-- and degree two nodes are pruned with 'pruneWith'.
--
-- Assume that the trees are valid!
intersectWith ::
  (Eq e, Ord a) => (e -> e -> e) -> [Tree e a] -> [Tree e a]
intersectWith f ts =
  if S.null ls
    then error "intersectWith: Intersection of leaves is empty."
    else map (retainLeavesWith f ls) ts
  where
    -- Leaf sets.
    lss = map (S.fromList . leaves) ts
    -- Common leaf set.
    ls = foldl1' S.intersection lss

-- Retain all leaves in a provided set; or conversely, drop all leaves not in a
-- provided set.
retainLeavesWith ::
  (Eq e, Ord a) => (e -> e -> e) -> Set a -> Tree e a -> Tree e a
retainLeavesWith f ls t = S.foldl' (flip (dropLeafWith f)) t leavesToDrop
  where
    leavesToDrop = S.fromList (leaves t) S.\\ ls

-- -- TODO: Is this needed?
-- -- | Merge two trees with the same topology. Returns 'Nothing' if the topologies
-- -- are different.
-- merge :: Tree e a -> Tree e b -> Maybe (Tree e (a, b))
-- merge (Node brL lbL xsL) (Node brR lbR xsR) =
--   if length xs == length ys
--     then -- I am proud of that :)).
--       zipWithM merge xs ys >>= Just . Node (l, r)
--     else Nothing

-- -- TODO: Is this needed?
-- -- | Apply a function with different effect on each node to a 'Traversable'.
-- -- Based on https://stackoverflow.com/a/41523456.
-- tZipWith :: Traversable t => (a -> b -> c) -> [a] -> t b -> Maybe (t c)
-- tZipWith f xs = sequenceA . snd . mapAccumL pair xs
--   where
--     pair [] _ = ([], Nothing)
--     pair (y : ys) z = (ys, Just (f y z))

-- | Each node of a tree is root of a subtree. Get the leaves of the subtree of
-- each node.
partitionTree :: (Ord a) => Tree e a -> Tree e (Set a)
-- I am proud of this awesome 'Comonad' usage here :).
partitionTree = extend (S.fromList . leaves)

-- -- | Get subtree of 'Tree' with nodes satisfying predicate. Return 'Nothing', if
-- -- no leaf satisfies predicate. At the moment: recursively, for each child, take
-- -- the child if any leaf in the child satisfies the predicate.
-- subTree :: (a -> Bool) -> Tree e a -> Maybe (Tree e a)
-- subTree p leaf@(Node br lb [])
--   | p lb = Just leaf
--   | otherwise = Nothing
-- subTree p (Node br lb chs) =
--   if null subTrees
--     then Nothing
--     else Just $ Node lbl subTrees
--   where
--     subTrees = mapMaybe (subTree p) chs

-- -- | Loop through each tree in a forest to report the complementary leaf sets.
-- subForestGetSubsets ::
--   (Ord a) =>
--   -- | Complementary partition at the stem
--   Set a ->
--   -- | Tree with partition nodes
--   Tree e (Set a) ->
--   [S.Set a]
-- subForestGetSubsets lvs t = lvsOthers
--   where
--     xs = subForest t
--     nChildren = length xs
--     lvsChildren = map rootLabel xs
--     lvsOtherChildren =
--       [ S.unions $ lvs : take i lvsChildren ++ drop (i + 1) lvsChildren
--         | i <- [0 .. (nChildren - 1)]
--       ]
--     lvsOthers = map (S.union lvs) lvsOtherChildren

-- -- | Check if a tree is bifurcating. A Bifurcating tree only contains degree one
-- -- and degree three nodes. I know, one should use a proper data structure to
-- -- encode bifurcating trees.
-- bifurcating :: Tree e a -> Bool
-- bifurcating (Node _ _ []) = True
-- bifurcating (Node _ _ [x, y]) = bifurcating x && bifurcating y
-- bifurcating (Node _ _ [_]) = False
-- bifurcating (Node _ _ _) = False

-- -- | Get clades induced by multifurcations.
-- clades :: Ord a => Tree e a -> [S.Set a]
-- clades (Node _ _ []) = []
-- clades (Node _ _ [x]) = clades x
-- clades (Node _ _ [x, y]) = clades x ++ clades y
-- clades t = S.fromList (leaves t) : concatMap clades (subForest t)
