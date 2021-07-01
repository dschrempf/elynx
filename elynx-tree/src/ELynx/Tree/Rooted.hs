{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  ELynx.Tree.Rooted
-- Description :  Rooted trees with labeled branches
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 09:57:29 2019.
--
-- Rooted 'Tree's are classical rose trees.
--
-- For rooted topologies, please see 'ELynx.Topology.Rooted'.
--
-- A 'Tree' is defined as:
--
-- @
-- data Tree a = Node
--   { rootLabel :: a,
--     subForest :: Forest a
--   }
-- @
--
-- where
--
-- @
-- type Forest a = [Tree a]
-- @
--
-- Here, aliases 'label' and 'forest' for 'rootLabel', and 'subForest' are
-- provided, respectively. This means, that the word 'Node' is reserved for the
-- constructor of a tree, and that a 'Node' has an attached 'label', and a
-- 'forest'. The terms /Node/ and /label/ referring to the value constructor
-- 'Node' and the record function 'label', respectively, are not to be confused.
-- The elements of the sub-forest are often called /children/.
--
-- In mathematical terms: A 'Tree' is a directed acyclic graph without loops,
-- with vertex labels.
--
-- A short recap of recursive tree traversals:
--
-- - Pre-order: Root first, then sub trees from left to right. Also called depth
--   first.
--
-- - In-order: Only valid for bifurcating trees. Left sub tree first, then root,
--   then right sub tree.
--
-- - Post-order: Sub trees from left to right, then the root. Also called
--   breadth first.
--
-- Here, pre-order traversals are used exclusively, for example, by accessor
-- functions such as 'labels', which is the same as 'toList'.
module ELynx.Tree.Rooted
  ( -- * Data type
    Tree (..),
    label,
    forest,
    Forest,
    labelL,
    forestL,

    -- * Access leaves, branches and labels
    leaves,
    duplicateLeaves,
    labels,
    setLabels,
    identify,

    -- * Structure
    degree,
    depth,
    pruneWith,
    dropNodesWith,
    dropLeavesWith,
    zipTreesWith,
    zipTrees,

    -- * ZipTrees
    ZipTree (..),
  )
where

import Control.Applicative
import Control.Comonad
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Data
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Tree
import GHC.Generics
import Lens.Micro

-- | Alias for 'rootLabel'.
label :: Tree a -> a
label = rootLabel

-- | Alias for 'subForest'.
forest :: Tree a -> [Tree a]
forest = subForest

-- forall f . Functor f => (a -> f b) -> Tree a -> f (Tree b)

-- | Access and modify the root label.
labelL :: Lens' (Tree a) a
labelL f (Node lb ts) = flip Node ts <$> f lb

-- forall f . Functor f => ([Tree a] -> f [Tree b]) -> Tree a -> f (Tree b)

-- | Access and modify the sub forest.
forestL :: Lens' (Tree a) [Tree a]
forestL f (Node lb ts) = Node lb <$> f ts

-- | Get the leaves of a tree.
leaves :: Tree a -> [a]
leaves (Node lb []) = [lb]
leaves (Node _ ts) = concatMap leaves ts

duplicates :: Ord a => [a] -> Bool
duplicates = go S.empty
  where
    go _ [] = False
    go seen (x : xs) = x `S.member` seen || go (S.insert x seen) xs

-- | Check if a tree has duplicate leaves.
duplicateLeaves :: Ord a => Tree a -> Bool
duplicateLeaves = duplicates . leaves

-- | Get node labels in pre-order.
labels :: Foldable t => t a -> [a]
labels = toList

-- | Set node labels in pre-order.
--
-- Return 'Nothing' if the provided list of labels is too short.
setLabels :: Traversable t => [a] -> t b -> Maybe (t a)
setLabels xs = sequenceA . snd . mapAccumL setLabel xs
  where
    setLabel [] _ = ([], Nothing)
    setLabel (y : ys) _ = (ys, Just y)

-- | Label the nodes with unique integers starting at the root with 0.
identify :: Traversable t => t a -> t Int
identify = snd . mapAccumL (\i _ -> (i + 1, i)) (0 :: Int)

-- | Degree of the root node.
--
-- The degree of a node is the number of branches attached to the node.
degree :: Tree a -> Int
degree = (+ 1) . length . forest

-- | Depth of a tree.
--
-- The [depth of a tree](https://en.wikipedia.org/wiki/Tree-depth) is the
-- largest number of nodes traversed on a path from the root to a leaf.
--
-- By convention, the depth is larger equal 1. That is, the depth of a leaf tree
-- is 1.
depth :: Tree a -> Int
depth = maximum . go 1
  where
    go n (Node _ []) = [n]
    go n (Node _ xs) = concatMap (go (n + 1)) xs

-- | Prune degree two nodes.
--
-- The information stored in pruned nodes is combined according to a given
-- function of the form @\daughterLabel parentLabel -> combinedLabel@.
pruneWith :: (a -> a -> a) -> Tree a -> Tree a
pruneWith _ t@(Node _ []) = t
pruneWith f (Node paLb [Node daLb daTs]) = Node (f daLb paLb) daTs
pruneWith f (Node paLb paTs) = Node paLb $ map (pruneWith f) paTs

-- | Drop nodes satisfying predicate.
--
-- Also drop parent nodes of which all daughter nodes are dropped.
--
-- Degree two nodes may arise.
--
-- Return 'Nothing' if:
--
-- - The root node satisfies the predicate.
--
-- - All daughter nodes of the root are dropped.
dropNodesWith :: (a -> Bool) -> Tree a -> Maybe (Tree a)
dropNodesWith p (Node lb ts)
  | p lb = Nothing
  | otherwise =
    if null ts'
      then Nothing
      else Just $ Node lb ts'
  where
    ts' = mapMaybe (dropNodesWith p) ts

-- | Drop leaves satisfying predicate.
--
-- Degree two nodes may arise.
--
-- Also drop parent nodes of which all leaves are dropped.
--
-- Return 'Nothing' if all leaves satisfy the predicate.
dropLeavesWith :: (a -> Bool) -> Tree a -> Maybe (Tree a)
dropLeavesWith p (Node lb [])
  | p lb = Nothing
  | otherwise = Just $ Node lb []
dropLeavesWith p (Node lb ts) =
  if null ts'
    then Nothing
    else Just $ Node lb ts'
  where
    ts' = mapMaybe (dropLeavesWith p) ts

-- | Zip two trees with the same topology.
--
-- This function differs from 'ZipTree' in that it fails when the topologies
-- don't match.
--
-- Return 'Nothing' if the topologies are different.
zipTreesWith ::
  (a1 -> a2 -> a) ->
  Tree a1 ->
  Tree a2 ->
  Maybe (Tree a)
zipTreesWith f (Node lbL tsL) (Node lbR tsR) =
  if length tsL == length tsR
    then -- I am proud of that :)).
      zipWithM (zipTreesWith f) tsL tsR >>= Just . Node (f lbL lbR)
    else Nothing

-- | See 'zipTreesWith'.
zipTrees :: Tree a1 -> Tree a2 -> Maybe (Tree (a1, a2))
zipTrees = zipTreesWith (,)

-- | The following newtype provides a zip-like applicative instance, similar to
-- 'Control.Applicative.ZipList'.
--
-- The default instance is not zip-like, because zip-like instances makes the
-- Monad instance meaningless (similar to lists).
newtype ZipTree a = ZipTree {getZipTree :: Tree a}
  deriving (Eq, Read, Show, Data, Generic, Generic1)

deriving instance Functor ZipTree

deriving instance Foldable ZipTree

instance Traversable ZipTree where
  traverse f (ZipTree t) = ZipTree <$> traverse f t

instance Comonad ZipTree where
  duplicate (ZipTree t) = ZipTree $ ZipTree <$> duplicate t
  extract = label . getZipTree

deriving instance NFData a => NFData (ZipTree a)

deriving instance ToJSON a => ToJSON (ZipTree a)

deriving instance FromJSON a => FromJSON (ZipTree a)

-- TODO: ZipTree instances: MonadFix (?), Alternative (?, see below).

-- instance Alternative ZipList where
--   empty = ZipList []
--   ZipList xs <|> ZipList ys = ZipList (xs ++ drop (length xs) ys)

-- |
-- >>> let t = ZipTree $ Node "" 0 [Node "" 1 [], Node "" 2 []] :: ZipTree String Int
-- >>> let f = ZipTree $ Node "+3" (+3) [Node "*5" (*5) [], Node "+10" (+10) []] :: ZipTree String (Int -> Int)
-- >>> f <*> t
--
-- ZipTree {getZipTree = Node {branch = "+3", label = 3, forest = [Node {branch = "*5", label = 5, forest = []},Node {branch = "+10", label = 12, forest = []}]}}
instance Applicative ZipTree where
  -- Infinite layers with infinite subtrees.
  pure lb = ZipTree $ Node lb $ repeat (pure lb)
  ~(ZipTree (Node lbF tsF)) <*> ~(ZipTree (Node lbX tsX)) =
    ZipTree $ Node (lbF lbX) (zipWith (<*>) tsF tsX)
  liftA2 f ~(ZipTree (Node lbX tsX)) ~(ZipTree (Node lbY tsY)) =
    ZipTree $ Node (f lbX lbY) (zipWith (liftA2 f) tsX tsY)
  ~(ZipTree (Node _ tsX)) *> ~(ZipTree (Node lbY tsY)) =
    ZipTree $ Node lbY (zipWith (*>) tsX tsY)
  ~(ZipTree (Node lbX tsX)) <* ~(ZipTree (Node _ tsY)) =
    ZipTree $ Node lbX (zipWith (<*) tsX tsY)
