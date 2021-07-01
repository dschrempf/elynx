{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Support
-- Description :  Labels with support values
-- Copyright   :  (c) Dominik Schrempf 2021
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
    HasMaybeSupport (..),

    -- * Functions on trees
    normalizeBranchSupport,
    collapse,
  )
where

import Control.DeepSeq
import Data.Aeson
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

instance HasMaybeSupport Support where
  getMaybeSupport = Just
  setMaybeSupport = const

instance HasSupport Support where
  getSupport = id
  modifySupport f = f

-- | Return 'Left' if negative.
toSupport :: Double -> Either String Support
toSupport x
  | x < 0 = Left $ "Support is negative: " ++ show x ++ "."
  | otherwise = Right $ Support x

-- | Do not check if value is negative.
toSupportUnsafe :: Double -> Support
toSupportUnsafe = Support

-- | Class of data types that may provide a support.
class HasMaybeSupport a where
  getMaybeSupport :: a -> Maybe Support
  setMaybeSupport :: Support -> a -> a

-- | Class of data types with measurable and modifiable support values.
class HasMaybeSupport a => HasSupport a where
  getSupport :: a -> Support

  setSupport :: Support -> a -> a
  setSupport = setMaybeSupport

  modifySupport :: (Support -> Support) -> a -> a

-- | Normalize branch support values. The maximum branch support value will be
-- set to 1.0.
normalizeBranchSupport :: HasSupport a => Tree a -> Tree a
normalizeBranchSupport t = modifySupport (/ m) <$> t
  where
    m = maximum $ getSupport <$> t

-- | Collapse branches with support lower than given value.
--
-- The branch and node labels of the collapsed branches are discarded.
collapse :: (Eq a, HasSupport a) => Support -> Tree a -> Tree a
collapse th tr =
  let tr' = collapse' th tr
   in if tr == tr' then tr else collapse th tr'

-- A leaf has full support.
highP :: HasSupport a => Support -> Tree a -> Bool
highP _ (Node _ []) = True
highP th (Node lb _) = getSupport lb >= th

-- See 'collapse'.
collapse' :: HasSupport a => Support -> Tree a -> Tree a
collapse' th (Node lb ts) = Node lb $ map (collapse' th) (highSupport ++ lowSupportForest)
  where
    (highSupport, lowSupport) = partition (highP th) ts
    lowSupportForest = concatMap forest lowSupport
