{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

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
module ELynx.Tree.Supported
  ( Support (fromSupport),
    toSupport,
    toSupportUnsafe,
    Supported (..),
    normalizeSupport,
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

-- | Non-negative branch support.
--
-- However, non-negativity is only checked with 'toSupport', and negative values
-- can be obtained using the 'Num' and related instances.
--
-- See also the documentation of 'ELynx.Tree.Measurable.Length'.
newtype Support = Support {fromSupport :: Double}
  deriving (Read, Show, Generic, NFData)
  deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real, RealFloat, RealFrac) via Double
  deriving (Semigroup) via Min Double

instance Splittable Support where
  split = id

instance ToJSON Support

instance FromJSON Support

instance Supported Support where
  getSup = id
  setSup = const
  modSup f = f

-- | Nothing if support is negative.
toSupport :: Double -> Either String Support
toSupport x | x < 0 = Left $ "toSupport: Branch support is negative: " ++ show x ++ "."
            | otherwise = Right $ Support x

-- | Do not check if support value is negative.
toSupportUnsafe :: Double -> Support
toSupportUnsafe = Support

-- | A branch label that supports extraction, setting and modifying of branch
-- support values.
class Supported e where
  getSup :: e -> Support
  setSup :: Support -> e -> e
  -- For computational efficiency.
  modSup :: (Support -> Support) -> e -> e

-- | Normalize branch support values. The maximum branch support value will be
-- set to 1.0.
normalizeSupport :: Supported e => Tree e a -> Tree e a
normalizeSupport t = first (modSup (/ m)) t
  where
    m = bimaximum $ bimap getSup (const 0) t

-- | Collapse branches with support lower than given value.
--
-- The branch and node labels of the collapsed branches are discarded.
collapse :: (Eq e, Eq a, Supported e) => Support -> Tree e a -> Tree e a
collapse th tr =
  let tr' = collapse' th tr
   in if tr == tr' then tr else collapse th tr'

-- A leaf has full support.
highP :: Supported e => Support -> Tree e a -> Bool
highP _ (Node _ _ []) = True
highP th (Node br _ _) = getSup br >= th

-- See 'collapse'.
collapse' :: Supported e => Support -> Tree e a -> Tree e a
collapse' th (Node br lb ts) = Node br lb $ map (collapse' th) (highSupport ++ lowSupportForest)
  where
    (highSupport, lowSupport) = partition (highP th) ts
    lowSupportForest = concatMap forest lowSupport
