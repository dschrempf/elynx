{- |
Module      :  ELynx.Data.Tree.Tree
Description :  Functions related to phylogenetic trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 09:57:29 2019.

Comment about nomenclature:

- In "Data.Tree", a 'Tree' is defined as

@
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }
@

This means, that the word 'Node' is reserved for the constructor of a tree, and
that a 'Node' has a label and a children. The terms 'Node' and /label/ are not
to be confused.

- Branches have /lengths/. For example, a branch length can be a distances or a
  time.

NOTE: Trees in this library are all rooted. Unrooted trees can be treated with a
rooted data structure equally well. However, in these cases, some functions have
no meaning. For example, functions measuring the distance from the root to the
leaves (the height of a rooted tree).

NOTE: Try fgl or alga. Use functional graph library for unrooted trees see also
the book /Haskell high performance programming from Thomasson/, p. 344.

-}


module ELynx.Data.Tree.Tree
  ( singleton
  , degree
  , leaves
  , subTree
  , subSample
  , nSubSamples
  , pruneWith
  , merge
  , tZipWith
  , partitionTree
  , subForestGetPartitions
  , bifurcating
  , roots
  , connect
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Maybe
import qualified Data.Sequence           as Seq
import qualified Data.Set                as Set
import           Data.Traversable
import           Data.Tree
import           System.Random.MWC

import           ELynx.Tools.Random
import           ELynx.Data.Tree.Partition

-- | The simplest tree. Usually an extant leaf.
singleton :: a -> Tree a
singleton l = Node l []

-- | The degree of the root node of a tree.
degree :: Tree a -> Int
degree = (+ 1) . length . subForest

-- | Get leaves of tree.
leaves :: Tree a -> [a]
leaves (Node l []) = [l]
leaves (Node _ f)  = concatMap leaves f

-- -- | Check if ancestor and daughters of first tree are a subset of the ancestor
-- -- and daughters of the second tree. Useful to test if, e.g., speciations agree.
-- rootNodesAgreeWith :: (Ord c) => (a -> c) -> Tree a -> (b -> c) -> Tree b -> Bool
-- rootNodesAgreeWith f s g t =
--   f (rootLabel s) == g (rootLabel t) &&
--   S.fromList sDs `S.isSubsetOf` S.fromList tDs
--   where sDs = map (f . rootLabel) (subForest s)
--         tDs = map (g . rootLabel) (subForest t)

-- | Get subtree of 'Tree' with nodes satisfying predicate. Return 'Nothing', if
-- no leaf satisfies predicate. At the moment: recursively, for each child, take
-- the child if any leaf in the child satisfies the predicate.
subTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
subTree p leaf@(Node lbl [])
  | p lbl     = Just leaf
  | otherwise = Nothing
subTree p (Node lbl chs) = if null subTrees
                           then Nothing
                           else Just $ Node lbl subTrees
  where subTrees = mapMaybe (subTree p) chs

-- XXX: If module gets too big, move the sampling functions into their own
-- module.
-- | Extract a random sub tree with N leaves of a tree with M leaves, where M>N
-- (otherwise error). The complete list of leaves (names are assumed to be
-- unique) has to be provided as a 'Seq.Seq', and a 'Seq.Set', so that we have
-- fast sub-sampling as well as lookup and don't have to recompute them when
-- many sub-samples are requested.
subSample :: (PrimMonad m, Ord a)
  => Seq.Seq a -> Int -> Tree a -> Gen (PrimState m) -> m (Maybe (Tree a))
subSample lvs n tree g
  | Seq.length lvs < n = error "Given list of leaves is shorter than requested number of leaves."
  | otherwise = do
      sampledLs <- sample lvs n g
      let ls = Set.fromList sampledLs
      return $ subTree (`Set.member` ls) tree

-- | See 'subSample', but n times.
nSubSamples :: (PrimMonad m, Ord a)
            => Int -> Seq.Seq a -> Int -> Tree a -> Gen (PrimState m) -> m [Maybe (Tree a)]
nSubSamples nS lvs nL tree g = replicateM nS $ subSample lvs nL tree g

-- | Prune or remove degree 2 inner nodes. The information stored in a pruned
-- node can be used to change the daughter node. To discard this information,
-- use, @pruneWith const tree@, otherwise @pruneWith (\daughter parent ->
-- combined) tree@.
pruneWith :: (a -> a -> a) -> Tree a -> Tree a
pruneWith _  n@(Node _ [])       = n
pruneWith f    (Node paLbl [ch]) = let lbl = f (rootLabel ch) paLbl
                                   in pruneWith f $ Node lbl (subForest ch)
pruneWith f    (Node paLbl chs)  = Node paLbl (map (pruneWith f) chs)

-- | Merge two trees with the same topology. Returns 'Nothing' if the topologies are different.
merge :: Tree a -> Tree b -> Maybe (Tree (a, b))
merge (Node l xs) (Node r ys) =
  if length xs == length ys
  -- I am proud of that :)).
  then zipWithM merge xs ys >>= Just . Node (l, r)
  else Nothing

-- | Apply a function with different effect on each node to a 'Traversable'.
-- Based on https://stackoverflow.com/a/41523456.
tZipWith :: Traversable t => (a -> b -> c) -> [a] -> t b -> Maybe (t c)
tZipWith f xs = sequenceA . snd . mapAccumL pair xs
    where pair [] _     = ([], Nothing)
          pair (y:ys) z = (ys, Just (f y z))

-- | Each node of a tree is root of a subtree. Get the leaves of the subtree of
-- each node.
partitionTree :: (Ord a) => Tree a -> Tree (Partition a)
partitionTree (Node l []) = Node (psingleton l) []
partitionTree (Node _ xs) = Node (punions $ map rootLabel xs') xs'
  where xs' = map partitionTree xs

-- | Loop through each tree in a forest to report the complementary leaf sets.
subForestGetPartitions :: (Ord a)
                     => Partition a          -- ^ Complementary partition at the stem
                     -> Tree (Partition a)   -- ^ Tree with partition nodes
                     -> [Partition a]
subForestGetPartitions lvs t = lvsOthers
  where
    xs               = subForest t
    nChildren        = length xs
    lvsChildren      = map rootLabel xs
    lvsOtherChildren = [ punions $ lvs
                         : take i lvsChildren ++ drop (i+1) lvsChildren
                       | i <- [0 .. (nChildren - 1)] ]
    lvsOthers        = map (punion lvs) lvsOtherChildren

-- | Check if a tree is bifurcating and does not include degree two nodes. I
-- know, one should use a proper data structure to encode bifurcating trees, but
-- I don't have enough time for this now.
bifurcating :: Tree a -> Bool
bifurcating (Node _ []    ) = True
bifurcating (Node _ [_]   ) = False
bifurcating (Node _ [x, y]) = bifurcating x && bifurcating y
bifurcating (Node _ _     ) = False

-- TODO: This bifurcating stuff irritates me. There are two solutions:
--
-- 1. Use a bifurcating data structure.
--
-- 2. Do not move down multifurcations.
--
-- Solution 1 is pretty, but doesn't allow for what we actually want to do,
-- which is solution 2. We want to encode clades that for sure do not contain
-- the root as multifurcations.

-- | For a rooted, bifurcating tree, get all possible rooted trees. For a tree
-- with @n>2@ leaves, there are @(2n-3)@ rooted trees. Beware, a bifurcating
-- tree without degree two nodes is assumed (see 'bifurcating'). The root node
-- is moved.
roots :: Tree a -> [Tree a]
-- Leaves, and cherries have to be handled separately, because they cannot be
-- rotated.
roots t@(Node _ [])                     = [t]
roots t@(Node _ [Node _ [], Node _ []]) = [t]
-- TODO: SOMETHING IS WRONG HERE.
roots t | bifurcating t = [t] ++ left t ++ right t
        | otherwise     = error "roots: Tree is not bifurcating."

-- Move the root to the left.
left :: Tree a -> [Tree a]
left   (Node i [Node j [x]   , z]) = left $ Node i [x           , Node j [z]  ]
left   (Node i [Node j [x, y], z]) = left  (Node i [x           , Node j [y,z]]) ++
                                     right (Node i [Node j [x,z], y           ])
-- TODO: OR HERE. THE NUMBERS OF TREES DO NOT MATCH WITH THE CONNECT FUNCTION.
left t@(Node _  [Node _ []   , _]) = [t]
left   (Node _ [] )                = error "left: Encountered a leaf."
left   _                           = error "left: Tree is not bifurcating."

-- Move the root to the right.
right :: Tree a -> [Tree a]
right   (Node i [x, Node j [z]   ]) = right $ Node i [Node j [x]  , z  ]
right   (Node i [x, Node j [y, z]]) = left  (Node i [y           , Node j [x,z]]) ++
                                      right (Node i [Node j [x,y], z           ])
right t@(Node _ [_, Node _ []   ]) = [t]
right   (Node _ [] )                = error "right: Encountered a leaf."
right   (Node _ [_])                = error "right: TODO; this case has to be handled separately."
right   _                           = error "left: Tree is not bifurcating."

-- | Connect two trees in all possible ways.
--
-- Basically, introduce a branch between two trees. If the trees have n, and m
-- branches, respectively, there are n*m ways to connect them.
--
-- A base node has to be given which will be used wherever the new node is
-- introduced.
connect :: a -> Tree a -> Tree a -> [Tree a]
connect n l r = [ Node n [x, y] | x <- roots l, y <- roots r]
