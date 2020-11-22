{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Support
-- Description :  Labels with support values
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jun 13 14:06:45 2019.
module ELynx.Tree.Support
  ( -- * Non-negative support value
    Support (fromSupport),
    toSupport,
    toSupportUnsafe,
    HasSupport (..),

    -- * Functions on trees
    normalizeBranchSupport,
    collapse,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.List
import Data.Semigroup
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import GHC.Generics

-- | Non-negative support value.
--
-- However, non-negativity is only checked with 'toSupport', and negative values
-- can be obtained using the 'Num' and related instances.
--
-- See also the documentation of 'ELynx.Tree.Length.Length'.
newtype Support = Support {fromSupport :: Double}
  deriving (Read, Show, Generic, NFData)
  deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real, RealFloat, RealFrac) via Double
  deriving (Semigroup) via Min Double

instance Splittable Support where
  split = id

instance ToJSON Support

instance FromJSON Support

instance HasSupport Support where
  getSup = id
  setSup = const
  modSup f = f

-- | If negative, call 'error' indicating the calling function name.
toSupport :: String -> Double -> Support
toSupport s x
  | x < 0 = error $ s ++ ": Support is negative: " ++ show x ++ "."
  | otherwise = Support x

-- | Do not check if value is negative.
toSupportUnsafe :: Double -> Support
toSupportUnsafe = Support

-- | A data type with measurable and modifiable values.
class HasSupport e where
  getSup :: e -> Support
  setSup :: Support -> e -> e

  -- For computational efficiency.
  modSup :: (Support -> Support) -> e -> e

-- | Normalize branch support values. The maximum branch support value will be
-- set to 1.0.
normalizeBranchSupport :: HasSupport e => Tree e a -> Tree e a
normalizeBranchSupport t = first (modSup (/ m)) t
  where
    m = bimaximum $ bimap getSup (const 0) t

-- | Collapse branches with support lower than given value.
--
-- The branch and node labels of the collapsed branches are discarded.
collapse :: (Eq e, Eq a, HasSupport e) => Support -> Tree e a -> Tree e a
collapse th tr =
  let tr' = collapse' th tr
   in if tr == tr' then tr else collapse th tr'

-- A leaf has full support.
highP :: HasSupport e => Support -> Tree e a -> Bool
highP _ (Node _ _ []) = True
highP th (Node br _ _) = getSup br >= th

-- See 'collapse'.
collapse' :: HasSupport e => Support -> Tree e a -> Tree e a
collapse' th (Node br lb ts) = Node br lb $ map (collapse' th) (highSupport ++ lowSupportForest)
  where
    (highSupport, lowSupport) = partition (highP th) ts
    lowSupportForest = concatMap forest lowSupport
