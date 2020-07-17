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
  ( Topology (..),
    degree,
    leaves,
  )
where

import Control.Applicative
import Control.DeepSeq
import Data.Data
import Data.Foldable
import GHC.Generics

-- | Rooted topologies with leaf labels.
data Topology a = Node { forest :: Forest a }
                | Leaf { label :: a }
                deriving (Eq, Read, Show, Data, Generic)

type Forest a = [Topology a]

instance Functor Topology where
  fmap f (Node ts) = Node $ map (fmap f) ts
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

  (Node tsF) <*> tx = Node $ map (<*> tx) tsF
  (Leaf lbF) <*> tx = lbF <$> tx

  liftA2 f (Node tsX) ty = Node $ map (\tx -> liftA2 f tx ty) tsX
  liftA2 f (Leaf lbX) (Node tsY) = Node $ map (f lbX <$>) tsY
  liftA2 f (Leaf lbX) (Leaf lbY) = Leaf $ f lbX lbY

  (Node tsX) *> ty@(Node tsY) = Node $ tsY ++ map (*> ty) tsX
  (Leaf _) *> (Node tsY) = Node tsY
  _ *> (Leaf lbY) = Leaf lbY

  (Node tsX) <* ty = Node $ map (<* ty) tsX
  (Leaf lbX) <* _ = Leaf lbX

-- TODO: This type checks, but I doubt the implementation is bug-free.
instance Monad Topology where
  (Node ts) >>= f = Node $ map (>>= f) ts
  (Leaf lb) >>= f = case f lb of
    Node ts' -> Node ts'
    Leaf lb' -> Leaf lb'

instance NFData a => NFData (Topology a) where
  rnf (Node ts) = rnf ts
  rnf (Leaf lb) = rnf lb

instance (ToJSON e, ToJSON a) => ToJSON (Tree e a)

instance (FromJSON e, FromJSON a) => FromJSON (Tree e a)

-- | The degree of the root node.
degree :: Topology a -> Int
degree (Node ts) = (+ 1) $ length ts
degree (Leaf _) = 1

-- | Set of leaves.
leaves :: Ord a => Topology a -> [a]
leaves (Leaf lb)  = [lb]
leaves (Node ts) = concatMap leaves ts

-- | Return leaf labels in pre-order.
flatten :: Topology a -> [a]
flatten t = squish t []
  where
    squish (Node ts) xs = foldr squish xs ts
    squish (Leaf lb) xs = lb : xs

-- TODO: Provide tests and arbitrary instances.

-- TODO: Provide conversion functions.
-- -- | Convert a tree to a topology. Internal node labels are lost.
-- fromTree :: Ord a => Tree a -> Topology a
-- fromTree (Node _ xs) = TN (S.fromList $ map fromTree xs)

-- TODO.
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

-- TODO.
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

-- -- | Connect two trees with a branch in all possible ways.
-- --
-- -- Introduce a branch between two trees. If the trees have @n>2@, and @m>2@
-- -- nodes, respectively, there are (n-2)*(m-2) ways to connect them.
-- --
-- -- A base node label has to be given which will be used wherever the new node is
-- -- introduced.
-- --
-- -- Branch labels are not handled.
-- --
-- -- Return 'Left' if one tree has a non-bifurcating root node.
-- connect :: a -> Tree () a -> Tree () a -> Either String (Forest () a)
-- connect lb l r = do
--   ls <- roots l
--   rs <- roots r
--   return [Node () lb [x, y] | x <- ls, y <- rs]
