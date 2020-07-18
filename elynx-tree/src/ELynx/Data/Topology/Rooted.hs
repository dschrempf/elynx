{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      :  ELynx.Data.Topology.Rooted
-- Description :  Topologies
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jul 11 10:28:28 2020.
--
-- A 'Topology' differs from a classical rose 'Data.Tree.Tree' in that it does
-- not have internal node labels. The leaves have labels.
--
-- For rooted trees, please see 'ELynx.Data.Tree.Rooted'.
--
-- In phylogenetics, the order of children of a topology node is arbitrary.
-- Internally, however, the underlying 'Tree' data structure stores the
-- sub-forest as a list, which has a specific order. Hence, we have to do some
-- tricks when comparing topologies, and topology comparison is slow.
module ELynx.Data.Topology.Rooted
  ( -- * Data type
    Topology (..),
    Forest,
    fromTree,
    fromLabeledTree,

    -- * Functions
    degree,
    leaves,
    flatten,
    identify,
    prune,
    dropLeavesWith,
    zipTreesWith,
    zipTrees,
    duplicateLeaves,
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Data
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Maybe
import qualified Data.Set as S
import Data.Traversable
import qualified Data.Tree as T
import qualified ELynx.Data.Tree.Rooted as R
import GHC.Generics

singleton :: NonEmpty a -> Bool
singleton xs = 1 == length (N.take 2 xs)

-- | Rooted topologies with leaf labels.
data Topology a
  = Node {forest :: Forest a}
  | Leaf {label :: a}
  deriving (Eq, Read, Show, Data, Generic)

type Forest a = NonEmpty (Topology a)

instance Functor Topology where
  fmap f (Node ts) = Node $ fmap (fmap f) ts
  fmap f (Leaf lb) = Leaf $ f lb

instance Foldable Topology where
  foldMap f (Node ts) = foldMap (foldMap f) ts
  foldMap f (Leaf lb) = f lb

  null _ = False
  {-# INLINE null #-}

  toList = flatten
  {-# INLINE toList #-}

instance Traversable Topology where
  traverse g (Node ts) = Node <$> traverse (traverse g) ts
  traverse g (Leaf lb) = Leaf <$> g lb

-- TODO: This type checks, but I doubt the implementation is bug-free.
instance Applicative Topology where
  pure = Leaf

  (Node tsF) <*> tx = Node $ fmap (<*> tx) tsF
  (Leaf lbF) <*> tx = lbF <$> tx

  liftA2 f (Node tsX) ty = Node $ fmap (\tx -> liftA2 f tx ty) tsX
  liftA2 f (Leaf lbX) (Node tsY) = Node $ fmap (f lbX <$>) tsY
  liftA2 f (Leaf lbX) (Leaf lbY) = Leaf $ f lbX lbY

  (Node tsX) *> ty@(Node tsY) = Node $ tsY <> fmap (*> ty) tsX
  (Leaf _) *> (Node tsY) = Node tsY
  _ *> (Leaf lbY) = Leaf lbY

  (Node tsX) <* ty = Node $ fmap (<* ty) tsX
  (Leaf lbX) <* _ = Leaf lbX

-- TODO: This type checks, but I doubt the implementation is bug-free.
instance Monad Topology where
  (Node ts) >>= f = Node $ fmap (>>= f) ts
  (Leaf lb) >>= f = case f lb of
    Node ts' -> Node ts'
    Leaf lb' -> Leaf lb'

instance NFData a => NFData (Topology a) where
  rnf (Node ts) = rnf ts
  rnf (Leaf lb) = rnf lb

instance ToJSON a => ToJSON (Topology a)

instance FromJSON a => FromJSON (Topology a)

-- | The degree of the root node.
degree :: Topology a -> Int
degree (Node ts) = (+ 1) $ length ts
degree (Leaf _) = 1

-- | Set of leaves.
leaves :: Ord a => Topology a -> [a]
leaves (Leaf lb) = [lb]
leaves (Node ts) = concatMap leaves ts

-- | Return leaf labels in pre-order.
flatten :: Topology a -> [a]
flatten t = squish t []
  where
    squish (Node ts) xs = foldr squish xs ts
    squish (Leaf lb) xs = lb : xs

-- TODO: Provide and fix tests, provide arbitrary instances.

-- | Convert a rooted rose tree to a rooted topology. Internal node labels are lost.
fromTree :: T.Tree a -> Topology a
fromTree (T.Node lb []) = Leaf lb
fromTree (T.Node _ xs) = Node $ fromTree <$> N.fromList xs

-- | Convert a rooted, labeled rose tree to a rooted topology. Branch labels and
-- internal node labels are lost.
fromLabeledTree :: R.Tree e a -> Topology a
fromLabeledTree (R.Node _ lb []) = Leaf lb
fromLabeledTree (R.Node _ _ xs) = Node $ fromLabeledTree <$> N.fromList xs

-- | Label the leaves with unique integers starting at 0.
identify :: Traversable t => t a -> t Int
identify = snd . mapAccumL (\i _ -> (i + 1, i)) (0 :: Int)

-- | Prune degree two nodes.
prune :: Topology a -> Topology a
prune (Node ts)
  | singleton ts = Node $ fmap prune $ forest $ N.head ts
  | otherwise = Node $ fmap prune ts
prune (Leaf lb) = Leaf lb

-- | Drop leaves satisfying predicate.
--
-- Degree two nodes may arise.
--
-- Return 'Nothing' if all leaves satisfy the predicate.
dropLeavesWith :: (a -> Bool) -> Topology a -> Maybe (Topology a)
dropLeavesWith p (Leaf lb)
  | p lb = Nothing
  | otherwise = Just $ Leaf lb
dropLeavesWith p (Node ts) =
  if null ts'
    then Nothing
    -- XXX: May be slow, unnecessary conversion to and from list.
    else Just $ Node $ N.fromList ts'
  where
    ts' = catMaybes $ N.toList $ fmap (dropLeavesWith p) ts

-- | Zip leaves of two equal topologies.
--
-- Return 'Nothing' if the topologies are different.
zipTreesWith :: (a1 -> a2 -> a) -> Topology a1 -> Topology a2 -> Maybe (Topology a)
zipTreesWith f (Node tsL) (Node tsR) =
  if N.length tsL == N.length tsR
    then
      -- XXX: May be slow, unnecessary conversion to and from list.
      zipWithM (zipTreesWith f) (N.toList tsL) (N.toList tsR) >>= Just . Node . N.fromList
    else Nothing
zipTreesWith f (Leaf lbL) (Leaf lbR) = Just $ Leaf $ f lbL lbR
zipTreesWith _ _ _ = Nothing

-- | Zip leaves of two equal topologies.
--
-- Return 'Nothing' if the topologies are different.
zipTrees :: Topology a1 -> Topology a2 -> Maybe (Topology (a1, a2))
zipTrees = zipTreesWith (,)

duplicates :: Ord a => [a] -> Bool
duplicates = go S.empty
  where
    go _ [] = False
    go seen (x : xs) = x `S.member` seen || go (S.insert x seen) xs

-- | Check if a topology has duplicate leaves.
duplicateLeaves :: Topology a -> Bool
duplicateLeaves = duplicates . leaves

-- -- | Remove multifurcations.
-- --
-- -- A caterpillar like bifurcating tree is used to resolve all multifurcations on
-- -- a tree. The multifurcating nodes are copied.
-- --
-- -- Branch labels are not handled.
-- resolve :: Tree () a -> Tree () a
-- resolve t@(Node _ _ []) = t
-- resolve (Node _ l [x]) = Node () l [resolve x]
-- resolve (Node _ l [x, y]) = Node () l $ map resolve [x, y]
-- resolve (Node _ l (x : xs)) = Node () l $ map resolve [x, Node () l xs]

-- outgroup :: Tree () a -> Tree () a

-- -- | For a rooted tree with a bifurcating root node, get all possible rooted
-- -- trees.
-- --
-- -- The root node is moved.
-- --
-- -- For a tree with @l=2@ leaves, there is one rooted tree. For a bifurcating
-- -- tree with @l>2@ leaves, there are @(2l-3)@ rooted trees. For a general tree
-- -- with a bifurcating root node, and a total number of @n>2@ nodes, there are
-- -- (n-2) rooted trees.
-- --
-- -- Moving a multifurcating root node to another branch would change the
-- -- topology, and so, a bifurcating root is required. To resolve a multifurcating
-- -- root, please see and use TODO.
-- --
-- -- Branch labels are not handled, but see 'rootsBranch'.
-- --
-- -- 'rootAt' roots the tree at a specific position.
-- --
-- -- Return 'Left' if the root node is not 'bifurcating'.
-- roots :: Tree () a -> Either String (Forest () a)
-- roots (Node _ _ []) = Left "roots: Root node is a leaf."
-- roots (Node _ _ [_]) = Left "roots: Root node has degree two."
-- roots t@(Node _ c [tL, tR]) = Right $ t : descend id () c tR tL ++ descend id () c tL tR
-- roots _ = Left "roots: Root node is multifurcating."

-- -- | Root a tree at a specific position.
-- --
-- -- Root the tree at the branch defined by the given bipartition. The original
-- -- root node is moved to the new position.
-- --
-- -- The root node must be bifurcating (see 'roots').
-- --
-- -- Branch labels are not handled, but see 'rootAtBranch'.
-- --
-- -- Return 'Left', if:
-- -- - the root node is not bifurcating;
-- -- - the tree has duplicate leaves;
-- -- - the bipartition does not match the leaves of the tree.
-- rootAt :: Ord a => Bipartition a -> Tree () a -> Either String (Tree () a)
-- rootAt = rootAtBranch id
