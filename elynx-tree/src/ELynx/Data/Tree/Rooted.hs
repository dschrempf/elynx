{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      :  ELynx.Data.Tree.Rooted
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
-- Rooted rose 'Tree's with labeled branches.
--
-- Comment about nomenclature: A 'Tree' is defined as
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
-- and that a 'Node' has an attached 'branch', a 'label', and a sub-'forest'.
-- The terms /Node/ and /label/ are not to be confused. The elements of the
-- sub-forest are often called /children/.
--
-- Using the 'Tree' data type has some disadvantages:
--
-- 1. All trees are rooted. Unrooted trees can be treated with a rooted data
-- structure, as it is used here. However, some functions may be meaningless.
--
-- 2. Changing branch labels, node labels, or the topology of the tree are slow
-- operations, especially, when the changes are close to the leaves of the tree.
--
-- Note: 'Tree's are rooted!
--
-- In mathematical terms: A 'Tree' is a directed acyclic graph without loops,
-- with vertex labels, with edge labels. Let me know if this definition is
-- incomplete.
module ELynx.Data.Tree.Rooted
  ( Tree (..),
    Forest,
    singleton,
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
    subTree,
    bifurcating,
    clades,
    roots,
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

-- TODO: Check usage of Forest = [Tree].

-- | Rooted rose trees with branch labels.
--
-- NOTE: This tree is rooted.
--
-- NOTE: Unary instances such as 'Functor' act on node labels, and not on branch
-- labels. Binary instances such as 'Bifunctor' act on both labels.
--
-- NOTE: Lifted instances are not provided.
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

-- -- XXX: Cannot provide MonadZip instance because branch labels cannot be
-- -- recovered from combined label.
-- instance Monoid e => MonadZip (Tree e) where
--   mzipWith f (Node brL lbL tsL) (Node brR lbR tsR) =
--     Node (brL <> brR) (f lbL lbR) (mzipWith (mzipWith f) tsL tsR)

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

-- | The degree of the root node.
degree :: Tree e a -> Int
degree = (+ 1) . length . forest

-- | Get leaves.
leaves :: Tree e a -> [a]
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

-- TODO: Use Monoid instance of @e@, not a dedicaated function.

-- | Prune degree two nodes.
--
-- The information stored in a pruned node is lost. The branches are combined
-- according to their 'Semigroup' instance of the form @\daughterBranch
-- parentBranch -> combinedBranch@.
pruneWith :: Semigroup e => Tree e a -> Tree e a
pruneWith t@(Node _ _ []) = t
pruneWith (Node paBr _ [Node daBr daLb daXs]) = Node (daBr <> paBr) daLb daXs
pruneWith (Node paBr paLb paXs) = Node paBr paLb $ map pruneWith paXs

-- | Drop a leaf from a tree.
--
-- The possibly resulting degree two node is pruned and the branches are
-- combined using their 'Semigroup' instance (see 'pruneWith').
--
-- The same tree is returned, if the leaf is not found on the tree.
dropLeafWith :: (Semigroup e, Eq a) => a -> Tree e a -> Tree e a
dropLeafWith l (Node paBr paLb paXs) =
  case paXs' of
    [Node daBr daLb daXs] -> Node (daBr <> paBr) daLb daXs
    _ -> Node paBr paLb paXs'
  where
    toRm x = null (forest x) && label x == l
    paXs' = map (dropLeafWith l) (filter (not . toRm) paXs)

-- | Compute the intersection of trees.
--
-- The intersections are the largest subtrees sharing the same leaf set. Leaf
-- are compared using a given function. Leaves are dropped with 'dropLeafWith',
-- and degree two nodes are pruned with 'pruneWith'.
--
-- Assume that the trees are valid!
intersectWith ::
  (Semigroup e, Eq e, Ord a) => [Tree e a] -> [Tree e a]
intersectWith ts =
  if S.null ls
    then error "intersectWith: Intersection of leaves is empty."
    else map (retainLeavesWith ls) ts
  where
    -- Leaf sets.
    lss = map (S.fromList . leaves) ts
    -- Common leaf set.
    ls = foldl1' S.intersection lss

-- Retain all leaves in a provided set; or conversely, drop all leaves not in a
-- provided set.
retainLeavesWith ::
  (Semigroup e, Eq e, Ord a) => Set a -> Tree e a -> Tree e a
retainLeavesWith ls t = S.foldl' (flip dropLeafWith) t leavesToDrop
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

-- | Each node of a tree is root of an induced subtree. Set the node labels to
-- the leaves of the induced subtrees.
partitionTree :: Tree e a -> Tree e [a]
-- I am proud of this awesome 'Comonad' usage here :).
partitionTree = extend leaves

-- | Get subtree of 'Tree' including leaves satisfying predicate.
--
-- - The resulting tree may contain degree 2 nodes.
-- - Return 'Nothing', if no leaf satisfies predicate.
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

-- | Check if a tree is bifurcating.
--
-- A Bifurcating tree only contains degree one and degree three nodes.
bifurcating :: Tree e a -> Bool
bifurcating (Node _ _ []) = True
bifurcating (Node _ _ [x, y]) = bifurcating x && bifurcating y
bifurcating _ = False

-- | Get clades induced by multifurcations.
--
-- A multifurcation is a node with three or more children (degree 4 or larger).
--
-- Collect the leaves of all trees induced by multifurcations.
clades :: Tree e a -> [[a]]
clades (Node _ _ []) = []
clades (Node _ _ [x]) = clades x
clades (Node _ _ [x, y]) = clades x ++ clades y
clades t = leaves t : concatMap clades (forest t)

-- -- TODO: Probably provide these functions also for 'Topology'.

-- | For a rooted, bifurcating tree, get all possible rooted (bifurcating) trees.
--
-- For a tree with @n>2@ leaves, there are @(2n-3)@ rooted trees. The root node
-- is moved. Branch labels are not handled (yet).
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

-- TODO: Continue here.

-- -- | Root a bifurcating tree at a given point.
-- --
-- -- Root the tree at the midpoint of the branch defined by the given bipartition.
-- -- The original root node is moved to the new position.
-- --
-- -- - The tree has to be bifurcating (may be relaxed in the future).
-- -- - The leaves of the tree have to be unique.
-- -- - The leaves in the bipartition have to match the leaves of the tree.
-- rootAt :: Ord a => Bipartition (PhyloLabel a) -> Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- rootAt b t
--   -- Tree is checked for being bifurcating in 'roots'.
--   | length ls /= S.size lS = error "rootAt: Leaves of tree are not unique."
--   | bS /= lS = error "rootAt: Bipartition does not match leaves of tree."
--   | otherwise =
--     fromMaybe
--       (error "rootAt: Bipartition not found on tree.")
--       (rootAt' b t)
--   where
--     bS = toSet b
--     ls = S.fromList $ leaves t
--     lS = S.fromList $ leaves t

-- -- Assume the leaves of the tree are unique.
-- rootAt' :: (Eq a, Ord a) => Bipartition (PhyloLabel a) -> Tree (PhyloLabel a) -> Maybe (Tree (PhyloLabel a))
-- rootAt' b t = find (\x -> b == bipartition x) (roots t)

-- -- | Connect two trees with a branch in all possible ways.
-- --
-- -- Basically, introduce a branch between two trees. If the trees have n, and m
-- -- branches, respectively, there are n*m ways to connect them.
-- --
-- -- A base node has to be given which will be used wherever the new node is
-- -- introduced.
-- connect :: PhyloLabel a -> Tree (PhyloLabel a) -> Tree (PhyloLabel a) -> [Tree (PhyloLabel a)]
-- connect n l r = [Node n [x, y] | x <- roots l, y <- roots r]

-- -- | Remove multifurcations by copying multifurcating nodes and introducing
-- -- branches with length 0 and branch support 0.
-- removeMultifurcations :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- removeMultifurcations t@(Node _ []) = t
-- removeMultifurcations (Node l [x]) = Node l [removeMultifurcations x]
-- removeMultifurcations (Node l [x, y]) = Node l $ map removeMultifurcations [x, y]
-- removeMultifurcations (Node l (x : xs)) = Node l $ map removeMultifurcations [x, Node l' xs]
--   where
--     l' = l {brLen = 0, brSup = 0}

-- -- | Add branch lengths. Set branch support to the lower support value. Forget
-- -- the parent node label.
-- extend :: PhyloLabel a -> PhyloLabel a -> PhyloLabel a
-- extend da pa = da {brSup = min (brSup pa) (brSup da), brLen = brLen pa + brLen da}

-- -- | Prune degree 2 nodes. Use 'extend' and 'pruneWith'.
-- prune :: Tree (PhyloLabel a) -> Tree (PhyloLabel a)
-- prune = pruneWith extend
