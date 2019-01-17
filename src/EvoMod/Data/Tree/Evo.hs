{- |
Module      :  EvoMod.Data.Tree.Evo
Description :  Evolutionary nodes.
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 14:19:26 2019.

-}


module EvoMod.Data.Tree.Evo
  ( EvoNode (..)
  ) where

-- | An evolutionary node has some information about where it is on the tree,
-- and if it is 'extant', 'extinct', 'internal', or 'external'. The latter two
-- could also be determined from the tree. This could be species, genes or
-- individuals; probably more.
class EvoNode n where
  extant          :: n -> Bool
  extinct         :: n -> Bool

  internal        :: n -> Bool
  internal n = not $ extant n || extinct n
  external        :: n -> Bool
  external   = not . internal
