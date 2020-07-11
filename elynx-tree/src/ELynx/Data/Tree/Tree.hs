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
-- Functions to work with rooted, rose 'Tree's with labeled leaves. Although for
-- phylogenetic trees, the order of children is unimportant, equality checks and
-- other comparisons are sensitive to the order of children for the moment. The
-- reason is that the underlying data structure stores the children as a list,
-- which has a specific order.
--
-- Comment about nomenclature:
--
-- - A 'Tree' is defined as
--
-- @
-- data Tree a = Node {
--         rootLabel :: a,         -- ^ label value
--         subForest :: Forest a   -- ^ zero or more child trees
--     }
-- @
--
-- This means, that the word 'Node' is reserved for the constructor of a tree, and
-- that a 'Node' has a label and children. The terms 'Node' and /label/ are not to
-- be confused.
--
-- Using the 'Tree' data type has some disadvantages:
--
-- 1. Branch labels are not explicit.
--
-- 2. The order of the sub-forest does not matter for phylogenetic trees.
-- Equality checks will throw false negatives the compared trees only differ in
-- their orders of sub-trees.
--
-- For example:
-- @
-- -- | A rose tree with unordered sub-forest, branch labels of type @e@ and node
-- -- labels of type @a@.
-- data Tree e a = Node e a (Set (Tree e a))
-- @
--
-- Branches can have /lengths/. For example, a branch length can be a distance
-- or a given number of time units.
--
-- NOTE: Trees in this library are all rooted. Unrooted trees can be treated with a
-- rooted data structure, as it is used here. However, some functions may be
-- meaningless.
module ELynx.Data.Tree.Tree
  ( Tree (branch, label, children),
    singleton,
    degree,
    leaves,
    pruneWith,
    dropLeafWith,
    -- intersectWith,
    -- merge,
    -- tZipWith,
    -- partitionTree,
    -- subForestGetSubsets,
    -- subTree,
    -- bifurcating,
    -- clades,
  )
where

-- import Control.Monad (zipWithM)
import Data.Function
-- import Data.List
--   ( foldl',
--     foldl1',
--   )
-- import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

-- import Data.Traversable

-- Unpack a singleton. Fail if set is not a singleton.
unpack :: Set a -> a
unpack s
  | S.size s == 1 = S.elemAt 0 s
  | otherwise = error "unpack: Set is not a singleton."

data Tree e a = Node
  { branch :: e,
    label :: a,
    children :: Set (Tree e a)
  }
  deriving (Eq)

instance (Eq e, Ord a) => Ord (Tree e a) where
  compare = compare `on` leaves

-- | The simplest tree. Usually an extant leaf.
singleton :: e -> a -> Tree e a
singleton br lb = Node br lb S.empty

-- TODO: Provide construction functions checking uniqueness of leaves.

-- | The degree of the root node of a tree.
degree :: Tree e a -> Int
degree = (+ 1) . S.size . children

-- | Get leaves of tree.
leaves :: Ord a => Tree e a -> Set a
leaves (Node _ lb xs)
  | S.null xs = S.singleton lb
  | otherwise = S.unions $ S.map leaves xs

-- | Prune degree two nodes. The information stored in a pruned node is lost.
-- The branches are combined with a given function of the form @\daughterBranch
-- parentBranch -> combinedBranch@.
pruneWith :: (Eq e, Ord a) => (e -> e -> e) -> Tree e a -> Tree e a
pruneWith f t@(Node paBr paLb paXs) = case S.size paXs of
  0 -> t
  1 -> Node (f daBr paBr) daLb daXs
  _ -> Node paBr paLb $ S.map (pruneWith f) paXs
  where
    (Node daBr daLb daXs) = unpack paXs

-- | Drop a leaf from a tree. The possibly resulting degree two node is pruned
-- and the branches are combined using a given function (see 'pruneWith').
--
-- The same tree is returned, if the leaf is not found on the tree.
dropLeafWith :: (Eq e, Ord a) => (e -> e -> e) -> a -> Tree e a -> Tree e a
dropLeafWith f l (Node paBr paLb paXs)
  | S.size paXs' == 1 = let Node daBr daLb daXs = unpack paXs' in Node (f daBr paBr) daLb daXs
  | otherwise = Node paBr paLb paXs'
  where
    toRm x = S.null (children x) && label x == l
    paXs' = S.map (dropLeafWith f l) (S.filter (not . toRm) paXs)

-- -- | Compute the intersection of trees.
-- --
-- -- The intersections are the largest subtrees sharing the same leaf set. Leaf
-- -- are compared using a given function. Leaves are dropped with 'dropLeafWith',
-- -- and degree two nodes are pruned with 'pruneWith'.
-- intersectWith ::
--   (Show b, Ord b) => (a -> b) -> (a -> a -> a) -> [Tree e a] -> [Tree e a]
-- intersectWith f g ts =
--   if null ls
--     then error "intersect: intersection of leaves is empty."
--     else map (retainLeavesWith f g ls) ts
--   where
--     -- Leaf sets.
--     lss = map (S.fromList . leaves . fmap f) ts
--     -- Common leaf set.
--     ls = foldl1' S.intersection lss

-- -- Retain all leaves in a provided set; or conversely, drop all leaves not in a
-- -- provided set.
-- retainLeavesWith ::
--   (Show b, Ord b) => (a -> b) -> (a -> a -> a) -> S.Set b -> Tree e a -> Tree e a
-- retainLeavesWith f g ls t = foldl' (flip (dropLeafWith f g)) t leavesToDrop
--   where
--     leavesToDrop = filter (`S.notMember` ls) $ leaves $ fmap f t

-- -- | Merge two trees with the same topology. Returns 'Nothing' if the topologies
-- -- are different.
-- merge :: Tree e a -> Tree e b -> Maybe (Tree e (a, b))
-- merge (Node brL lbL xsL) (Node brR lbR xsR) =
--   if length xs == length ys
--     then -- I am proud of that :)).
--       zipWithM merge xs ys >>= Just . Node (l, r)
--     else Nothing

-- -- | Apply a function with different effect on each node to a 'Traversable'.
-- -- Based on https://stackoverflow.com/a/41523456.
-- tZipWith :: Traversable t => (a -> b -> c) -> [a] -> t b -> Maybe (t c)
-- tZipWith f xs = sequenceA . snd . mapAccumL pair xs
--   where
--     pair [] _ = ([], Nothing)
--     pair (y : ys) z = (ys, Just (f y z))

-- -- | Each node of a tree is root of a subtree. Get the leaves of the subtree of
-- -- each node.
-- partitionTree :: (Ord a) => Tree e a -> Tree e (Set a)
-- partitionTree (Node br lb xs) = Node (S.singleton l) []
-- partitionTree (Node br lb xs) = Node (S.unions $ map rootLabel xs') xs'
--   where
--     xs' = map partitionTree xs

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
