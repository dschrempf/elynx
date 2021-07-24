{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  ELynx.Topology.Rooted
-- Description :  Topologies
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jul 11 10:28:28 2020.
--
-- A rooted 'Topology' differs from a classical rooted rose 'Data.Tree.Tree' in
-- that it does not have internal node labels. The leaves have labels.
--
-- For rooted trees with branch labels, see "ELynx.Tree.Rooted". Please also see
-- the note about tree traversals therein.
module ELynx.Topology.Rooted
  ( -- * Data type
    Topology (..),
    Forest,
    fromRoseTree,
    fromBranchLabelTree,
    toBranchLabelTreeWith,

    -- * Access leaves, branches and labels
    leaves,
    duplicateLeaves,
    identify,

    -- * Structure
    degree,
    prune,
    dropLeavesWith,
    zipTreesWith,
    zipTrees,
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Data
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Maybe
import qualified Data.Set as S
import Data.Traversable
import qualified Data.Tree as T
import qualified ELynx.Tree.Rooted as R
import GHC.Generics

singleton :: NonEmpty a -> Bool
singleton xs = 1 == length (N.take 2 xs)

-- | Rooted topologies with leaf labels.
data Topology a
  = Node {forest :: Forest a}
  | Leaf {label :: a}
  deriving (Eq, Read, Show, Data, Generic)

-- | A shortcut.
type Forest a = NonEmpty (Topology a)

instance Functor Topology where
  fmap f (Node ts) = Node $ fmap (fmap f) ts
  fmap f (Leaf lb) = Leaf $ f lb

instance Foldable Topology where
  foldMap f (Node ts) = foldMap (foldMap f) ts
  foldMap f (Leaf lb) = f lb

  null _ = False
  {-# INLINE null #-}

  toList = leaves
  {-# INLINE toList #-}

instance Traversable Topology where
  traverse g (Node ts) = Node <$> traverse (traverse g) ts
  traverse g (Leaf lb) = Leaf <$> g lb

instance Applicative Topology where
  pure = Leaf

  (Node tsF) <*> tx = Node $ tsF <&> (<*> tx)
  (Leaf lbF) <*> tx = lbF <$> tx

  liftA2 f (Node tsX) ty = Node $ fmap (\tx -> liftA2 f tx ty) tsX
  liftA2 f (Leaf lbX) (Node tsY) = Node $ fmap (f lbX <$>) tsY
  liftA2 f (Leaf lbX) (Leaf lbY) = Leaf $ f lbX lbY

  (Node tsX) *> ty = Node $ tsX <&> (*> ty)
  (Leaf _) *> (Node tsY) = Node tsY
  (Leaf _) *> (Leaf y) = Leaf y

  (Node tsX) <* ty = Node $ tsX <&> (<* ty)
  (Leaf x) <* ty = x <$ ty

instance Monad Topology where
  (Node ts) >>= f = Node $ fmap (>>= f) ts
  (Leaf lb) >>= f = f lb

instance NFData a => NFData (Topology a) where
  rnf (Node ts) = rnf ts
  rnf (Leaf lb) = rnf lb

instance ToJSON a => ToJSON (Topology a)

instance FromJSON a => FromJSON (Topology a)

-- | Convert a rooted rose tree to a rooted topology. Internal node labels are lost.
fromRoseTree :: T.Tree a -> Topology a
fromRoseTree (T.Node lb []) = Leaf lb
fromRoseTree (T.Node _ xs) = Node $ fromRoseTree <$> N.fromList xs

-- | Convert a rooted, branch-label tree to a rooted topology. Branch labels and
-- internal node labels are lost.
fromBranchLabelTree :: R.Tree e a -> Topology a
fromBranchLabelTree (R.Node _ lb []) = Leaf lb
fromBranchLabelTree (R.Node _ _ xs) = Node $ fromBranchLabelTree <$> N.fromList xs

-- | Convert a rooted topology to a rooted, branch-label tree. Use the given
-- node label at internal nodes.
toBranchLabelTreeWith :: a -> Topology a -> R.Tree () a
toBranchLabelTreeWith _ (Leaf lb) = R.Node () lb []
toBranchLabelTreeWith x (Node ts) = R.Node () x $ map (toBranchLabelTreeWith x) $ N.toList ts

-- | Set of leaves.
leaves :: Topology a -> [a]
leaves (Leaf lb) = [lb]
leaves (Node ts) = concatMap leaves ts

-- -- NOTE: This implementation of 'leaves' may be faster.
-- -- | Return leaf labels in pre-order.
-- flatten :: Topology a -> [a]
-- flatten t = squish t []
--   where
--     squish (Node ts) xs = foldr squish xs ts
--     squish (Leaf lb) xs = lb : xs

duplicates :: Ord a => [a] -> Bool
duplicates = go S.empty
  where
    go _ [] = False
    go seen (x : xs) = x `S.member` seen || go (S.insert x seen) xs

-- | Check if a topology has duplicate leaves.
duplicateLeaves :: Ord a => Topology a -> Bool
duplicateLeaves = duplicates . leaves

-- TODO: This is the same as in ELynx.Tree.Rooted.

-- | Label the leaves with unique integers starting at 0.
identify :: Traversable t => t a -> t Int
identify = snd . mapAccumL (\i _ -> (i + 1, i)) (0 :: Int)

-- | The degree of the root node.
degree :: Topology a -> Int
degree (Node ts) = (+ 1) $ length ts
degree (Leaf _) = 1

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
    else -- XXX: May be slow, unnecessary conversion to and from list.
      Just $ Node $ N.fromList ts'
  where
    ts' = catMaybes $ N.toList $ fmap (dropLeavesWith p) ts

-- | Zip leaves of two equal topologies.
--
-- Return 'Nothing' if the topologies are different.
zipTreesWith :: (a1 -> a2 -> a) -> Topology a1 -> Topology a2 -> Maybe (Topology a)
zipTreesWith f (Node tsL) (Node tsR) =
  if N.length tsL == N.length tsR
    then -- XXX: May be slow, unnecessary conversion to and from list.
      zipWithM (zipTreesWith f) (N.toList tsL) (N.toList tsR) >>= Just . Node . N.fromList
    else Nothing
zipTreesWith f (Leaf lbL) (Leaf lbR) = Just $ Leaf $ f lbL lbR
zipTreesWith _ _ _ = Nothing

-- | Zip leaves of two equal topologies.
--
-- Return 'Nothing' if the topologies are different.
zipTrees :: Topology a1 -> Topology a2 -> Maybe (Topology (a1, a2))
zipTrees = zipTreesWith (,)
