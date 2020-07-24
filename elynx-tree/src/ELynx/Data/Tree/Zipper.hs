-- |
-- Module      :  ELynx.Data.Tree.Zipper
-- Description :  Zippers on rooted rose trees with branch labels
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jul 23 08:42:37 2020.
module ELynx.Data.Tree.Zipper
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
    -- modifyTree,
    -- modifyBranch,
    -- modifyLabel,
  )
where

import Data.Foldable
import ELynx.Data.Tree.Rooted

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

fromTree :: Tree e a -> TreePos e a
fromTree t = Pos {current = t, before = [], after = [], parents = []}

toTree :: TreePos e a -> Tree e a
toTree = current . goRoot

getForest :: TreePos e a -> Forest e a
getForest pos = foldl (flip (:)) (current pos : after pos) (before pos)

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

goRoot :: TreePos e a -> TreePos e a
goRoot pos = maybe pos goRoot (goUp pos)

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

goPath :: [Int] -> TreePos e a -> Maybe (TreePos e a)
goPath pos pth = foldlM (flip goChild) pth pos

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

unsafeGoPath :: [Int] -> TreePos e a -> TreePos e a
unsafeGoPath pos pth = foldl (flip unsafeGoChild) pth pos

insertTree :: Tree e a -> TreePos e a -> TreePos e a
insertTree t pos = pos {current = t}

insertBranch :: e -> TreePos e a -> TreePos e a
insertBranch br pos = case current pos of
  Node _ lb ts -> pos {current = Node br lb ts}

insertLabel :: a -> TreePos e a -> TreePos e a
insertLabel lb pos = case current pos of
  Node br _ ts -> pos {current = Node br lb ts}

-- modifyTree :: (Tree e a -> Tree e a) -> TreePos e a -> TreePos e a
-- modifyTree f pos = insertTree (f $ current pos) pos

-- modifyBranch :: (e -> e) -> TreePos e a -> TreePos e a
-- modifyBranch f pos = case current pos of
--   Node br lb ts -> pos {current = Node (f br) lb ts}

-- modifyLabel :: (a -> a) -> TreePos e a -> TreePos e a
-- modifyLabel f pos = case current pos of
--   Node br lb ts -> pos {current = Node br (f lb) ts}
