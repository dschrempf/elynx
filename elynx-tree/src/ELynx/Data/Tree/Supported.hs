-- |
-- Module      :  ELynx.Data.Tree.Supported
-- Description :  Branch label with support value
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jun 13 14:06:45 2019.
module ELynx.Data.Tree.Supported
  ( BranchSupport,
    Supported (..),
    normalizeBranchSupport,
    collapse,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import Data.List
import ELynx.Data.Tree.Rooted

-- | Branch support.
type BranchSupport = Double

-- | A branch label that supports extraction and setting of branch support values.
class Supported e where
  getBranchSupport :: e -> BranchSupport
  setBranchSupport :: BranchSupport -> e -> e

-- Apply a function to a branch support label.
apply :: Supported e => (BranchSupport -> BranchSupport) -> e -> e
apply f l = setBranchSupport (f s) l where s = getBranchSupport l

-- | Normalize branch support values. The maximum branch support value will be
-- set to 1.0.
normalizeBranchSupport :: Supported e => Tree e a -> Tree e a
normalizeBranchSupport t = first (apply (/ m)) t
  where m = bimaximum $ bimap getBranchSupport (const 0) t

-- TODO: Something was wrong here. @collapse 1.0 t@ should be a star tree but it
-- was a leaf. Is this still so?
-- | Collapse branches with support lower than given value.
--
-- The branch and node labels of the collapsed branches are discarded.
collapse :: (Eq e, Eq a, Supported e) => BranchSupport -> Tree e a -> Tree e a
collapse th tr = let tr' = collapse' th tr in
  if tr == tr' then tr else collapse th tr'

-- See 'collapse'.
collapse' :: Supported e => BranchSupport -> Tree e a -> Tree e a
collapse' _ t@(Node _ _ []) = t
collapse' th (Node br lb ts) = Node br lb $ map (collapse' th) (highSupport ++ lowSupportForest)
  where
    (highSupport, lowSupport) = partition ((>= th) . getBranchSupport . branch) ts
    lowSupportForest = concatMap forest lowSupport
