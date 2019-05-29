{- |
Module      :  EvoMod.Data.Tree.Tree
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
has a label and a children. The terms 'Node' and /label/ are not to be confused.

- Branches have /lengths/. For example, a branch length can be a distances or a
  time.

NOTE: Try fgl or alga. Use functional graph library for unrooted trees see also
the book /Haskell high performance programming from Thomasson/, p. 344.

-}


module EvoMod.Data.Tree.Tree
  ( singleton
  , degree
  , leaves
  , subTree
  , subSample
  , nSubSamples
  , pruneWith
  , bipartitions
  , symmetricDistance
  , incompatibleSplitsDistance
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Maybe
import qualified Data.Sequence           as Seq
import qualified Data.Set                as Set
import           Data.Tree
import           System.Random.MWC

import           EvoMod.Tools.Random

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

-- TODO: Probably move the sampling functions into their own module.
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
      sampledLeaves <- sample lvs n g
      let leavesSet = Set.fromList sampledLeaves
      return $ subTree (`Set.member` leavesSet) tree

-- | See 'subSample', but n times.
nSubSamples :: (PrimMonad m, Ord a)
            => Int -> Seq.Seq a -> Int -> Tree a -> Gen (PrimState m) -> m [Maybe (Tree a)]
nSubSamples nS lvs nL tree g = replicateM nS $ subSample lvs nL tree g

-- | Prune degree 2 inner nodes. The information stored in a pruned node can be
-- used to change the daughter node. To discard this information, use,
-- @pruneWith const tree@, otherwise @pruneWith (\daughter parent -> combined)
-- tree@.
pruneWith :: (a -> a -> a) -> Tree a -> Tree a
pruneWith _  n@(Node _ [])       = n
pruneWith f    (Node paLbl [ch]) = let lbl = f (rootLabel ch) paLbl
                                     in pruneWith f $ Node lbl (subForest ch)
pruneWith f    (Node paLbl chs)  = Node paLbl (map (pruneWith f) chs)

-- TODO: BUGGED! Leaves have to be passed to children.
-- | Get all bipartitions. XXX: This is slow at the moment, because 'leaves' is
-- called excessively.
bipartitions :: Tree a -> [([a], [a])]
bipartitions (Node _ []    ) = []
bipartitions (Node _ [c]   ) = bipartitions c
-- The crux is that we have to handle bifurcation in a special way. Otherwise,
-- we get our bipartitions twice!
bipartitions (Node _ [l, r]) = (leaves l, leaves r) : (bipartitions l ++ bipartitions r)
-- For a multifurcation, take each leaf set in turn, and contrast it with the
-- the concatenation of the other leaves.
bipartitions (Node _ xs    ) = bs ++ concatMap bipartitions xs
  where lss = map leaves xs
        bs  = [ (ls, concat $ take i lss ++ drop (i+1) lss) | (i, ls) <- zip [0..] lss ]

-- | Symmetric (Robinson-Foulds) distance between two trees. Assumes that the
-- leaves have unique names! XXX: Comparing a list of trees with this function
-- recomputes bipartitions.
symmetricDistance :: Eq a => Tree a -> Tree a -> Int
symmetricDistance t1 t2 = length b1NotInb2 + length b2NotInb1
  where b1 = bipartitions t1
        b2 = bipartitions t2
        b1NotInb2 = filter (`notElem` b2) b1
        b2NotInb1 = filter (`notElem` b1) b2

-- | Number of incompatible splits. Similar to 'symmetricDistance' but merges
-- multifurcations.
incompatibleSplitsDistance :: Eq a => Tree a -> Tree a -> Int
incompatibleSplitsDistance = undefined
