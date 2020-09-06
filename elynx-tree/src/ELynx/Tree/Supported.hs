-- |
-- Module      :  ELynx.Tree.Supported
-- Description :  Branch label with support value
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jun 13 14:06:45 2019.
--
-- Non-negativity of branch support values is not (yet) ensured. To ensure
-- non-negativity, a newtype wrapper could be used, but this would be a major
-- refactor.
module ELynx.Tree.Supported
  ( BranchSupport,
    Supported (..),
    normalizeBranchSupport,
    collapse,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import Data.List
import ELynx.Tree.Rooted

-- | Branch support.
type BranchSupport = Double

-- | A branch label that supports extraction and setting of branch support values.
class Supported e where
  getSup :: e -> BranchSupport
  setSup :: BranchSupport -> e -> e

-- Apply a function to a branch support label.
apply :: Supported e => (BranchSupport -> BranchSupport) -> e -> e
apply f l = setSup (f s) l where s = getSup l

-- | Normalize branch support values. The maximum branch support value will be
-- set to 1.0.
normalizeBranchSupport :: Supported e => Tree e a -> Tree e a
normalizeBranchSupport t = first (apply (/ m)) t
  where
    m = bimaximum $ bimap getSup (const 0) t

-- | Collapse branches with support lower than given value.
--
-- The branch and node labels of the collapsed branches are discarded.
collapse :: (Eq e, Eq a, Supported e) => BranchSupport -> Tree e a -> Tree e a
collapse th tr =
  let tr' = collapse' th tr
   in if tr == tr' then tr else collapse th tr'

-- A leaf has full support.
highP :: Supported e => Double -> Tree e a -> Bool
highP _ (Node _ _ []) = True
highP th (Node br _ _) = getSup br >= th

-- See 'collapse'.
collapse' :: Supported e => BranchSupport -> Tree e a -> Tree e a
collapse' th (Node br lb ts) = Node br lb $ map (collapse' th) (highSupport ++ lowSupportForest)
  where
    (highSupport, lowSupport) = partition (highP th) ts
    lowSupportForest = concatMap forest lowSupport
