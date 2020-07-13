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
-- comparisons between trees, the order of the trees in the sub-forest is
-- meaningless. However, the underlying data structure stores the sub-forest as
-- a list, which has a specific order.
--
-- Comment about nomenclature:
--
-- - A 'Tree' is defined as
--
-- @
-- data Tree e a = Node
--   { branch :: e,
--     label :: a,
--     forest :: [Tree e a]
--   }
-- @
--
-- This means, that the word 'Node' is reserved for the constructor of a tree,
-- and that a 'Node' has an attached branch, a label, and a sub-forest. The
-- terms 'Node' and /label/ are not to be confused.
--
-- Using the 'Tree' data type has some disadvantages:
--
-- 1. All trees are rooted. Unrooted trees can be treated with a rooted data
-- structure, as it is used here. However, some functions may be meaningless.
--
-- 2. Internally, the sub-forests are ordered, so we have to do some tricks when
-- comparing trees (see the 'Eq' instance).
--
-- 3. Changing branch labels, node labels, or the topology of the tree are slow
-- operations, especially, when the changes are close to the leaves of the tree.
--
-- 4. The uniqueness of the leaves has to be checked at runtime and is not
-- ensured by the data type.
--
-- NOTE: Trees in this library are all rooted.
module ELynx.Data.Tree.Tree
  ( Tree (branch, label, forest),
    singleton,
    valid,
    equalTopology,
    degree,
    leaves,
    flatten,
    labelNodes,
    pruneWith,
    dropLeafWith,
    intersectWith,
    zipTreesWith,
    zipTrees,
    partitionTree,
    -- subForestGetSubsets,
    subTree,
    -- bifurcating,
    -- clades,
  )
where

import Control.Applicative
import Control.Comonad
import Control.DeepSeq
import Control.Monad
import Control.Monad.Fix
import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Data
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics

-- | Rooted rose trees with branch labels.
--
-- NOTE: This tree is rooted.
--
-- NOTE: Unary instances such as 'Functor' act on node labels, and not on branch
-- labels. Binary instances such as 'Bifunctor' act on both labels.
data Tree e a = Node
  { branch :: e,
    label :: a,
    forest :: Forest e a
  }
  deriving (Read, Show, Data, Generic, Generic1)

-- | A shorthand.
type Forest e a = [Tree e a]

-- | Checking for equality is slow because the order of trees in the sub-forests
-- is arbitrary.
instance (Eq e, Eq a) => Eq (Tree e a) where
  ~(Node brL lbL tsL) == ~(Node brR lbR tsR) =
    (brL == brR)
      && (lbL == lbR)
      && (length tsL == length tsR)
      && all (`elem` tsR) tsL

-- | Map over node labels.
instance Functor (Tree e) where
  fmap f ~(Node br lb ts) = Node br (f lb) $ map (fmap f) ts
  x <$ ~(Node br _ ts) = Node br x (map (x <$) ts)

instance Bifunctor Tree where
  bimap f g ~(Node br lb ts) = Node (f br) (g lb) $ map (bimap f g) ts
  first f ~(Node br lb ts) = Node (f br) lb $ map (first f) ts
  second g ~(Node br lb ts) = Node br (g lb) $ map (second g) ts

-- | Combine node labels.
instance Foldable (Tree e) where
  foldMap f ~(Node _ lb ts) = f lb <> foldMap (foldMap f) ts
  null _ = False
  {-# INLINE null #-}
  toList = flatten
  {-# INLINE toList #-}

instance Bifoldable Tree where
  bifoldMap f g ~(Node br lb ts) = f br <> g lb <> foldMap (bifoldMap f g) ts

instance Traversable (Tree e) where
  traverse g ~(Node br lb ts) = Node br <$> g lb <*> traverse (traverse g) ts

instance Bitraversable Tree where
  bitraverse f g ~(Node br lb ts) = Node <$> f br <*> g lb <*> traverse (bitraverse f g) ts

-- NOTE: The following code provides a zip-like applicative instance. However,
-- the zip-like instance makes the Monad instance meaningless. So, either we
-- provide only 'Applicative' in zip-like form, or we use the classic instance
-- for 'Applicative' and 'Monad'.

-- -- | NOTE: The 'Applicative' instance of 'Tree' is similar to the one of
-- -- 'Control.Applicative.ZipList', and differs from the instance of
-- -- 'Data.Tree.Tree'!
-- --
-- -- >>> let t = Node "" 0 [Node "" 1 [], Node "" 2 []] :: Tree String Int
-- -- >>> let f = Node "+3" (+3) [Node "*5" (*5) [], Node "+10" (+10) []] :: Tree String (Int -> Int)
-- -- >>> f <*> t
-- -- Node {branch = "+3", label = 3, forest = [Node {branch = "*5", label = 5, forest = []},Node {branch = "+10", label = 12, forest = []}]}
-- --
-- -- NOTE: The 'Monoid' instance of the branch labels determines how the branches
-- -- are combined. For example, distances can be summed using the
-- -- 'Data.Monoid.Sum' monoid.
-- instance Monoid e => Applicative (Tree e) where
--   pure lb = Node mempty lb []
--   ~(Node brF lbF tsF) <*> ~(Node brX lbX tsX) =
--     Node (brF <> brX) (lbF lbX) (zipWith (<*>) tsF tsX)
--   liftA2 f ~(Node brX lbX tsX) ~(Node brY lbY tsY) =
--     Node (brX <> brY) (f lbX lbY) (zipWith (liftA2 f) tsX tsY)
--   ~(Node brX _ tsX) *> ~(Node brY lbY tsY) =
--     Node (brX <> brY) lbY (zipWith (*>) tsX tsY)
--   ~(Node brX lbX tsX) <* ~(Node brY _ tsY) =
--     Node (brX <> brY) lbX (zipWith (<*) tsX tsY)

-- | NOTE: The 'Monoid' instance of the branch labels determines how the
-- branches are combined. For example, distances can be summed usingthe
-- 'Data.Monoid.Sum'.
instance Monoid e => Applicative (Tree e) where
  pure lb = Node mempty lb []
  ~(Node brF lbF tsF) <*> ~tx@(Node brX lbX tsX) =
    Node (brF <> brX) (lbF lbX) (map (lbF <$>) tsX ++ map (<*> tx) tsF)
  liftA2 f ~(Node brX lbX tsX) ~ty@(Node brY lbY tsY) =
    Node (brX <> brY) (f lbX lbY) (map (f lbX <$>) tsY ++ map (\tx -> liftA2 f tx ty) tsX)
  ~(Node brX _ tsX) *> ~ty@(Node brY lbY tsY) =
    Node (brX <> brY) lbY (tsY ++ map (*> ty) tsX)
  ~(Node brX lbX tsX) <* ~ty@(Node brY _ tsY) =
    Node (brX <> brY) lbX (map (lbX <$) tsY ++ map (<* ty) tsX)

-- NOTE: A zip-like applicative instance would make the Monad instance meaningless.

-- | NOTE: The 'Monoid' instance of the branch labels determines how the
-- branches are combined. For example, distances can be summed using
-- 'Data.Monoid.Sum'.
instance Monoid e => Monad (Tree e) where
  ~(Node br lb ts) >>= f = case f lb of
    Node br' lb' ts' -> Node (br <> br') lb' (ts' ++ map (>>= f) ts)

-- instance Monoid e => MonadZip (Tree e) where
--   mzipWith f (Node brL lbL tsL) (Node brR lbR tsR) =
--     Node (brL <> brR) (f lbL lbR) (mzipWith (mzipWith f) tsL tsR)

-- -- XXX: Cannot provide MonadZip instance because branch labels cannot be
-- -- recovered from combined label.
--   munzip (Node br (lbL, lbR) ts) = (Node ? lbL tsL, Node ? lbR tsR)
--     where
--       (tsL, tsR) = munzip (map munzip ts)

instance Monoid e => MonadFix (Tree e) where
  mfix = mfixTree

mfixTree :: (a -> Tree e a) -> Tree e a
mfixTree f
  | Node br lb ts <- fix (f . label) =
    Node
      br
      lb
      ( zipWith
          (\i _ -> mfixTree ((!! i) . forest . f))
          [0 ..]
          ts
      )

instance Comonad (Tree e) where
  duplicate t@(Node br _ ts) = Node br t (map duplicate ts)
  extract (Node _ lb _) = lb
  {-# INLINE extract #-}

instance (NFData e, NFData a) => NFData (Tree e a) where
  rnf (Node br lb ts) = rnf br `seq` rnf lb `seq` rnf ts

instance (ToJSON e, ToJSON a) => ToJSON (Tree e a)

instance (FromJSON e, FromJSON a) => FromJSON (Tree e a)

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

-- | Check if two trees have the same topology.
equalTopology :: Eq a => Tree e a -> Tree e a -> Bool
equalTopology l r = rmBr l == rmBr r
  where
    rmBr = first (const ())

-- | The degree of the root node.
degree :: Tree e a -> Int
degree = (+ 1) . length . forest

-- | Get leaves.
leaves :: Ord a => Tree e a -> [a]
leaves (Node _ lb []) = [lb]
leaves (Node _ _ xs) = concatMap leaves xs

-- | Return node labels in pre-order.
flatten :: Tree e a -> [a]
flatten t = squish t []
  where
    squish (Node _ x ts) xs = x : foldr squish xs ts

-- | Label the nodes with unique integer ids starting at the root with 0. Works
-- for any 'Traversable' data type.
labelNodes :: Traversable t => t a -> t Int
labelNodes = snd . mapAccumL (\i _ -> (i + 1, i)) (0 :: Int)

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
    toRm x = null (forest x) && label x == l
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

-- | Zip two trees with the same topology. Returns 'Nothing' if the topologies
-- are different.
zipTreesWith ::
  (e1 -> e2 -> e) ->
  (a1 -> a2 -> a) ->
  Tree e1 a1 ->
  Tree e2 a2 ->
  Maybe (Tree e a)
zipTreesWith f g (Node brL lbL tsL) (Node brR lbR tsR) =
  if length tsL == length tsR
    then -- I am proud of that :)).
      zipWithM (zipTreesWith f g) tsL tsR >>= Just . Node (f brL brR) (g lbL lbR)
    else Nothing

-- | Zip two trees with the same topology. Returns 'Nothing' if the topologies
-- are different.
zipTrees :: Tree e1 a1 -> Tree e2 a2 -> Maybe (Tree (e1, e2) (a1, a2))
zipTrees = zipTreesWith (,) (,)

-- | Each node of a tree is root of a subtree. Get the leaves of the subtree of
-- each node.
partitionTree :: (Ord a) => Tree e a -> Tree e (Set a)
-- I am proud of this awesome 'Comonad' usage here :).
partitionTree = extend (S.fromList . leaves)

-- | Get subtree of 'Tree' with leaves satisfying predicate.
--
-- Return 'Nothing', if no leaf satisfies predicate.
subTree :: (a -> Bool) -> Tree e a -> Maybe (Tree e a)
subTree p leaf@(Node _ lb [])
  | p lb = Just leaf
  | otherwise = Nothing
subTree p (Node br lb ts) =
  if null subTrees
    then Nothing
    else Just $ Node br lb subTrees
  where
    subTrees = mapMaybe (subTree p) ts

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
