{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  ELynx.Tree.Rooted
-- Description :  Rooted trees with labeled branches

-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 09:57:29 2019.
--
-- Rooted 'Tree's differes from a classical rose 'Data.Tree.Tree' in that it has
-- labeled branches.
--
-- For rooted topologies, please see 'ELynx.Topology.Rooted'.
--
-- A 'Tree' is defined as:
--
-- @
-- data Tree e a = Node
--   { branch :: e,
--     label :: a,
--     forest :: Forest e a
--   }
-- @
--
-- where
--
-- @
-- type Forest e a = [Tree e a]
-- @
--
-- This means, that the word 'Node' is reserved for the constructor of a tree,
-- and that a 'Node' has an attached 'branch', a 'label', and a sub-'forest'.
-- The value constructor /Node/ and the record function /label/ are not to be
-- confused. The elements of the sub-forest are often called /children/.
--
-- With respect to phylogenetic analyses, using the 'Tree' data type has some
-- disadvantages:
--
-- 1. All trees are rooted. Unrooted trees can be treated with a rooted data
-- structure, as it is used here. However, some functions may be meaningless.
--
-- 2. Changing branch labels, node labels, or the topology of the tree are slow
-- operations, especially, when the changes are close to the leaves of the tree.
--
-- In mathematical terms: A 'Tree' is a directed acyclic graph without loops,
-- with vertex labels, with edge labels. Let me know if this definition is
-- incomplete.
module ELynx.Tree.Rooted
  ( -- * Data type
    Tree (..),
    Forest,
    toTreeBranchLabels,
    toTreeNodeLabels,

    -- * Access leaves, branches and labels
    leaves,
    duplicateLeaves,
    branches,
    setBranches,
    applyStem,
    labels,
    setLabels,
    applyRoot,
    identify,

    -- * Structure
    degree,
    prune,
    dropNodesWith,
    dropLeavesWith,
    zipTreesWith,
    zipTrees,
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
import qualified Data.Set as S
import qualified Data.Tree as T
import GHC.Generics

-- | Rooted rose trees with branch labels.
--
-- Unary instances such as 'Functor' act on node labels, and not on branch
-- labels. Binary instances such as 'Bifunctor' act on both labels (`first` acts
-- on branches, `second` on node labels).
--
-- Lifted instances are not provided.
data Tree e a = Node
  { branch :: e,
    label :: a,
    forest :: Forest e a
  }
  deriving (Eq, Read, Show, Data, Generic)

-- | A shorthand.
type Forest e a = [Tree e a]

-- | Map over node labels.
instance Functor (Tree e) where
  fmap f ~(Node br lb ts) = Node br (f lb) $ map (fmap f) ts
  x <$ ~(Node br _ ts) = Node br x (map (x <$) ts)

-- | The function 'first' acts on branch labels, 'second' on node labels.
instance Bifunctor Tree where
  bimap f g ~(Node br lb ts) = Node (f br) (g lb) $ map (bimap f g) ts
  first f ~(Node br lb ts) = Node (f br) lb $ map (first f) ts
  second g ~(Node br lb ts) = Node br (g lb) $ map (second g) ts

-- | Combine node labels in pre-order.
instance Foldable (Tree e) where
  foldMap f ~(Node _ lb ts) = f lb <> foldMap (foldMap f) ts
  null _ = False
  {-# INLINE null #-}
  toList = labels
  {-# INLINE toList #-}

instance Bifoldable Tree where
  bifoldMap f g ~(Node br lb ts) = f br <> g lb <> foldMap (bifoldMap f g) ts

instance Traversable (Tree e) where
  traverse g ~(Node br lb ts) = Node br <$> g lb <*> traverse (traverse g) ts

instance Bitraversable Tree where
  bitraverse f g ~(Node br lb ts) = Node <$> f br <*> g lb <*> traverse (bitraverse f g) ts

-- The following code provides a zip-like applicative instance. However,
-- the zip-like instance makes the Monad instance meaningless. So, either we
-- provide only 'Applicative' in zip-like form, or we use the classic instance
-- for 'Applicative' and 'Monad'.

-- -- | Note: The 'Applicative' instance of 'Tree' is similar to the one of
-- -- 'Control.Applicative.ZipList', and differs from the instance of
-- -- 'Data.Tree.Tree'!
-- --
-- -- >>> let t = Node "" 0 [Node "" 1 [], Node "" 2 []] :: Tree String Int
-- -- >>> let f = Node "+3" (+3) [Node "*5" (*5) [], Node "+10" (+10) []] :: Tree String (Int -> Int)
-- -- >>> f <*> t
-- -- Node {branch = "+3", label = 3, forest = [Node {branch = "*5", label = 5, forest = []},Node {branch = "+10", label = 12, forest = []}]}
-- --
-- -- Note: The 'Monoid' instance of the branch labels determines how the branches
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

-- | The 'Semigroup' instance of the branch labels determines how the
-- branches are combined. For example, distances can be summed using
-- 'Data.Semigroup.Sum'.
--
-- The 'Monoid' instance of the branch labels determines the default branch
-- label when using 'pure'.
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

-- | The 'Semigroup' instance of the branch labels determines how the branches
-- are combined. For example, distances can be summed using
-- 'Data.Semigroup.Sum'.
instance Monoid e => Monad (Tree e) where
  ~(Node br lb ts) >>= f = case f lb of
    Node br' lb' ts' -> Node (br <> br') lb' (ts' ++ map (>>= f) ts)

-- -- Cannot provide MonadZip instance because branch labels cannot be
-- -- recovered from combined label.
-- instance Monoid e => MonadZip (Tree e) where
--   mzipWith f (Node brL lbL tsL) (Node brR lbR tsR) =
--     Node (brL <> brR) (f lbL lbR) (mzipWith (mzipWith f) tsL tsR)
--
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

-- | Conversion to 'T.Tree' using branch labels.
toTreeBranchLabels :: Tree e a -> T.Tree e
toTreeBranchLabels (Node br _ ts) = T.Node br (map toTreeBranchLabels ts)

-- | Conversion to 'T.Tree' using node labels.
toTreeNodeLabels :: Tree e a -> T.Tree a
toTreeNodeLabels (Node _ lb ts) = T.Node lb (map toTreeNodeLabels ts)

-- | Get leaves.
leaves :: Tree e a -> [a]
leaves (Node _ lb []) = [lb]
leaves (Node _ _ ts) = concatMap leaves ts

-- | Check if a tree has duplicate leaves.
duplicateLeaves :: Ord a => Tree e a -> Bool
duplicateLeaves = duplicates . leaves

-- | Get branch labels in pre-order.
branches :: Tree e a -> [e]
branches t = squish t []
  where
    squish (Node br _ ts) xs = br : foldr squish xs ts

-- | Set branch labels in pre-order.
--
-- Return 'Nothing' if the provided list of branch labels is too short.
setBranches :: Bitraversable t => [f] -> t e a -> Maybe (t f a)
setBranches xs = bisequenceA . snd . bimapAccumL setBranch noChange xs
  where
    setBranch [] _ = ([], Nothing)
    setBranch (y : ys) _ = (ys, Just y)
    noChange ys z = (ys, Just z)

-- | Change the root branch of a tree.
applyStem :: (e -> e) -> Tree e a -> Tree e a
applyStem f t = t {branch = f $ branch t}

-- | Return node labels in pre-order.
labels :: Tree e a -> [a]
labels t = squish t []
  where
    squish (Node _ lb ts) xs = lb : foldr squish xs ts

-- | Set node labels in pre-order.
--
-- Return 'Nothing' if the provided list of node labels is too short.
setLabels :: Traversable t => [b] -> t a -> Maybe (t b)
setLabels xs = sequenceA . snd . mapAccumL setLabel xs
  where
    setLabel [] _ = ([], Nothing)
    setLabel (y : ys) _ = (ys, Just y)

-- | Change the root label of a tree.
applyRoot :: (a -> a) -> Tree e a -> Tree e a
applyRoot f t = t {label = f $ label t}

-- | Label the nodes with unique integers starting at the root with 0.
identify :: Traversable t => t a -> t Int
identify = snd . mapAccumL (\i _ -> (i + 1, i)) (0 :: Int)

-- | The degree of the root node.
degree :: Tree e a -> Int
degree = (+ 1) . length . forest

-- | Prune degree two nodes.
--
-- The information stored in a pruned node is lost. The branches are combined
-- according to their 'Semigroup' instance of the form @\daughterBranch
-- parentBranch -> combinedBranch@.
prune :: Semigroup e => Tree e a -> Tree e a
prune t@(Node _ _ []) = t
prune (Node paBr _ [Node daBr daLb daTs]) = Node (daBr <> paBr) daLb daTs
prune (Node paBr paLb paTs) = Node paBr paLb $ map prune paTs

-- | Drop nodes satisfying predicate.
--
-- Degree two nodes may arise.
--
-- Also drop parent nodes of which all daughter nodes are dropped.
--
-- Return 'Nothing' if the root node satisfies the predicate.
dropNodesWith :: (a -> Bool) -> Tree e a -> Maybe (Tree e a)
dropNodesWith p (Node br lb ts)
  | p lb = Nothing
  | otherwise =
    if null ts'
      then Nothing
      else Just $ Node br lb ts'
  where
    ts' = mapMaybe (dropNodesWith p) ts

-- | Drop leaves satisfying predicate.
--
-- Degree two nodes may arise.
--
-- Also drop parent nodes of which all leaves are dropped.
--
-- Return 'Nothing' if all leaves satisfy the predicate.
dropLeavesWith :: (a -> Bool) -> Tree e a -> Maybe (Tree e a)
dropLeavesWith p (Node br lb [])
  | p lb = Nothing
  | otherwise = Just $ Node br lb []
dropLeavesWith p (Node br lb ts) =
  if null ts'
    then Nothing
    else Just $ Node br lb ts'
  where
    ts' = mapMaybe (dropLeavesWith p) ts

-- | Zip two trees with the same topology.
--
-- Return 'Nothing' if the topologies are different.
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

-- | Zip two trees with the same topology.
--
-- Return 'Nothing' if the topologies are different.
zipTrees :: Tree e1 a1 -> Tree e2 a2 -> Maybe (Tree (e1, e2) (a1, a2))
zipTrees = zipTreesWith (,) (,)

duplicates :: Ord a => [a] -> Bool
duplicates = go S.empty
  where
    go _ [] = False
    go seen (x : xs) = x `S.member` seen || go (S.insert x seen) xs
