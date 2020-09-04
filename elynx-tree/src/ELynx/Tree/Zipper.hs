-- |
-- Module      :  ELynx.Tree.Zipper
-- Description :  Zippers on rooted rose trees with branch labels
-- Copyright   :  (c) Dominik Schrempf, 2020
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
    goUp,
    goRoot,
    goLeft,
    goRight,
    goChild,
    goPath,
    unsafeGoPath,

    -- * Modification
    insertTree,
    insertBranch,
    insertLabel,
  )
where

import Data.Foldable
import ELynx.Tree.Rooted

-- | Tree zipper. For reference, please see http://hackage.haskell.org/package/rosezipper.
data TreePos e a = Pos
  { -- | The currently selected tree.
    current :: Tree e a,
    -- | Forest to the left in reversed order.
    before :: Forest e a,
    -- | Forest to the right
    after :: Forest e a,
    -- | Finger to the selected tree
    parents :: [([Tree e a], e, a, [Tree e a])]
  }
  deriving (Show, Eq)

-- | Get a zipper pointing to the root.
fromTree :: Tree e a -> TreePos e a
fromTree t = Pos {current = t, before = [], after = [], parents = []}

-- | Get the complete tree of the zipper.
toTree :: TreePos e a -> Tree e a
toTree = current . goRoot

getForest :: TreePos e a -> Forest e a
getForest pos = foldl (flip (:)) (current pos : after pos) (before pos)

-- | Go to parent.
goUp :: TreePos e a -> Maybe (TreePos e a)
goUp pos = case parents pos of
  (ls, br, lb, rs) : ps ->
    Just
      Pos
        { current = Node br lb $ getForest pos,
          before = ls,
          after = rs,
          parents = ps
        }
  [] -> Nothing

-- | Go to root.
goRoot :: TreePos e a -> TreePos e a
goRoot pos = maybe pos goRoot (goUp pos)

-- | Go to left sibling in current forest.
goLeft :: TreePos e a -> Maybe (TreePos e a)
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
goRight :: TreePos e a -> Maybe (TreePos e a)
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
goChild :: Int -> TreePos e a -> Maybe (TreePos e a)
goChild n pos = case current pos of
  (Node br lb ts)
    | null ts -> Nothing
    | length ts <= n -> Nothing
    | otherwise ->
      Just $
        Pos
          { current = head rs',
            before = reverse ls',
            after = tail rs',
            parents = (before pos, br, lb, after pos) : parents pos
          }
    where
      (ls', rs') = splitAt n ts

-- | Go to node with given path.
goPath :: [Int] -> TreePos e a -> Maybe (TreePos e a)
goPath pos pth = foldlM (flip goChild) pth pos

-- | Go to child with given index in forest. Call 'error' if child does not
-- exist.
unsafeGoChild :: Int -> TreePos e a -> TreePos e a
unsafeGoChild n pos = case current pos of
  (Node br lb ts)
    | null ts -> error "unsafeGoChild: Forest is empty."
    | length ts <= n -> error "unsafeGoChild: Forest is too short."
    | otherwise ->
      Pos
        { current = head rs',
          before = reverse ls',
          after = tail rs',
          parents = (before pos, br, lb, after pos) : parents pos
        }
    where
      (ls', rs') = splitAt n ts

-- | Got to node with given path. Call 'error' if path is invalid.
unsafeGoPath :: [Int] -> TreePos e a -> TreePos e a
unsafeGoPath pos pth = foldl (flip unsafeGoChild) pth pos

-- | Insert a new tree into the current focus of the zipper.
insertTree :: Tree e a -> TreePos e a -> TreePos e a
insertTree t pos = pos {current = t}

-- | Insert a new branch label into the current focus of the zipper.
insertBranch :: e -> TreePos e a -> TreePos e a
insertBranch br pos = case current pos of
  Node _ lb ts -> pos {current = Node br lb ts}

-- | Insert a new node label into the current focus of the zipper.
insertLabel :: a -> TreePos e a -> TreePos e a
insertLabel lb pos = case current pos of
  Node br _ ts -> pos {current = Node br lb ts}
