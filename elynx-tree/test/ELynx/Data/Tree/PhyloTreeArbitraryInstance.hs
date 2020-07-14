{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      :  ELynx.Data.Tree.PhyloTreeArbitraryInstance
Description :  Arbitrary instance, needed for QuickCheck
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Apr 21 17:13:23 2020.

-}

module ELynx.Data.Tree.PhyloTreeArbitraryInstance
  ()
where

import           Test.QuickCheck

import           ELynx.Data.Tree

-- Of course, the boundaries for branch support and length have been chosen
-- pretty arbitrarily :).
--
-- XXX: This instance does not produce values without branch lengths nor branch
-- supports.
instance Arbitrary Phylo where
  arbitrary = Phylo <$> (Just <$> choose (1, 100)) <*> (Just <$> choose (0.001, 10))
