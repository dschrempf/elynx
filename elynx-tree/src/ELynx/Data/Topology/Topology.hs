-- |
-- Module      :  ELynx.Data.Topology.Topology
-- Description :  Topologies
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jul 11 10:28:28 2020.
--
-- A 'Topology' is a tree without branch labels and without internal node
-- labels. The leaves have unique labels.
--
-- The order of children is arbitrary. Internally, 'Set's are used instead of
-- lists like for rose trees (see 'Data.Tree').
module ELynx.Data.Topology.Topology
  ( Topology (..),
    fromTree,
    leaves,
  )
where

import Data.Function
import Data.Tree
import qualified Data.Set as S
import Data.Set (Set)

data Topology a = TN { children ::Set (Topology a) }
                | TL { label :: a }
                deriving (Eq)

instance Ord a => Ord (Topology a) where
  compare = compare `on` leaves

-- | Convert a tree to a topology. Internal node labels are lost.
fromTree :: Ord a => Tree a -> Topology a
fromTree (Node _ xs) = TN (S.fromList $ map fromTree xs)

-- | Set of leaves.
leaves :: Ord a => Topology a -> Set a
leaves (TN xs) = S.unions $ S.map leaves xs
leaves (TL x)  = S.singleton x

-- TODO: Probably provide 'roots', 'rootAt', 'connect' also for 'Topology'.
