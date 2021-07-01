-- |
-- Module      :  ELynx.Tree.Zipper
-- Description :  Zippers on rooted rose trees with branch labels
-- Copyright   :  (c) Dominik Schrempf, 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jul 23 08:42:37 2020.
module ELynx.Tree.Zipper
  ( -- * Data type
    TreePos (..),

    -- * Conversion
    fromTree,
    toTree,

    -- * Movement
    goParent,
    goParentUnsafe,
    goRoot,
    goLeft,
    goRight,
    goChild,
    goChildUnsafe,

    -- * Paths
    Path,
    goPath,
    goPathUnsafe,
    getSubTreeUnsafe,
    isValidPath,
    isLeafPath,

    -- * Modification
    insertTree,
    modifyTree,
    insertLabel,
  )
where

import Data.Foldable
import ELynx.Tree.Rooted

-- | Tree zipper. For reference, please see http://hackage.haskell.org/package/rosezipper.
data TreePos a = Pos
  { -- | The currently selected tree.
    current :: Tree a,
    -- | Sub-forest to the left in reversed order.
    before :: Forest a,
    -- | Sub-forest to the right
    after :: Forest a,
    -- | Finger to the selected tree
    parents :: [([Tree a], a, [Tree a])]
  }
  deriving (Show, Eq)

-- | Get a zipper pointing to the root.
fromTree :: Tree a -> TreePos a
fromTree t = Pos {current = t, before = [], after = [], parents = []}

-- | Get the complete tree of the zipper.
toTree :: TreePos a -> Tree a
toTree = current . goRoot

getSubForest :: TreePos a -> Forest a
getSubForest pos = foldl (flip (:)) (current pos : after pos) (before pos)

-- | Go to parent.
goParent :: TreePos a -> Maybe (TreePos a)
goParent pos = case parents pos of
  (ls, lb, rs) : ps ->
    Just
      Pos
        { current = Node lb $ getSubForest pos,
          before = ls,
          after = rs,
          parents = ps
        }
  [] -> Nothing

-- | Go to parent.
--
-- Call 'error' if no parent is found.
goParentUnsafe :: TreePos a -> TreePos a
goParentUnsafe pos = case parents pos of
  (ls, lb, rs) : ps ->
    Pos
      { current = Node lb $ getSubForest pos,
        before = ls,
        after = rs,
        parents = ps
      }
  [] -> error "goUpUnsafe: No parent found."

-- | Go to root.
goRoot :: TreePos a -> TreePos a
goRoot pos = maybe pos goRoot (goParent pos)

-- | Go to left sibling in current forest.
goLeft :: TreePos a -> Maybe (TreePos a)
goLeft pos =
  case before pos of
    t : ts ->
      Just
        pos
          { current = t,
            before = ts,
            after = current pos : after pos
          }
    [] -> Nothing

-- | Go to right sibling in current forest.
goRight :: TreePos a -> Maybe (TreePos a)
goRight pos =
  case after pos of
    t : ts ->
      Just
        pos
          { current = t,
            before = current pos : before pos,
            after = ts
          }
    [] -> Nothing

-- | Go to child with given index in forest.
goChild :: Int -> TreePos a -> Maybe (TreePos a)
goChild n pos = case current pos of
  (Node lb ts)
    | null ts -> Nothing
    | length ts <= n -> Nothing
    | otherwise ->
      Just $
        Pos
          { current = head rs',
            before = reverse ls',
            after = tail rs',
            parents = (before pos, lb, after pos) : parents pos
          }
    where
      (ls', rs') = splitAt n ts

-- | Go to child with given index in forest. Call 'error' if child does not
-- exist.
goChildUnsafe :: Int -> TreePos a -> TreePos a
goChildUnsafe n pos = case current pos of
  (Node lb ts)
    | null ts -> error "goChildUnsafe: Forest is empty."
    | length ts <= n -> error "goChildUnsafe: Forest is too short."
    | otherwise ->
      Pos
        { current = head rs',
          before = reverse ls',
          after = tail rs',
          parents = (before pos, lb, after pos) : parents pos
        }
    where
      (ls', rs') = splitAt n ts

-- | Path from the root of a tree to the node of the tree.
--
-- The position is specific to a tree topology. If the topology changes, the
-- position becomes invalid.
type Path = [Int]

-- | Go to node with given path.
goPath :: Path -> TreePos a -> Maybe (TreePos a)
goPath pos pth = foldlM (flip goChild) pth pos

-- | Check if a path is valid in that it leads to a node on a tree.
isValidPath :: Tree a -> Path -> Bool
isValidPath t p = case goPath p (fromTree t) of
  Nothing -> False
  Just _ -> True

-- | Check if a path leads to a leaf.
isLeafPath :: Tree a -> Path -> Bool
isLeafPath t p = case goPath p (fromTree t) of
  Nothing -> False
  Just pos -> null $ forest (current pos)

-- | Got to node with given path.
--
-- Call 'error' if path is invalid.
goPathUnsafe :: Path -> TreePos a -> TreePos a
goPathUnsafe pos pth =
  {-# SCC "goPathUnsafe" #-}
  foldl (flip goChildUnsafe) pth pos

-- | Get the sub tree at path.
--
-- Call 'error' if path is invalid.
getSubTreeUnsafe :: Path -> Tree a -> Tree a
getSubTreeUnsafe p = current . goPathUnsafe p . fromTree

-- | Insert a new tree into the current focus of the zipper.
insertTree :: Tree a -> TreePos a -> TreePos a
insertTree t pos = pos {current = t}

-- | Modify the tree at the current focus of the zipper.
modifyTree :: (Tree a -> Tree a) -> TreePos a -> TreePos a
modifyTree f pos = pos {current = f t}
  where
    t = current pos

-- | Insert a new node label into the current focus of the zipper.
insertLabel :: a -> TreePos a -> TreePos a
insertLabel lb pos = case current pos of
  Node _ ts -> pos {current = Node lb ts}
