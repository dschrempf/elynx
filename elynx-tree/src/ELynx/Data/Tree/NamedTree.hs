{- |
Module      :  ELynx.Data.Tree.NamedTree
Description :  Trees with named nodes
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 24 20:09:20 2019.

-}

module ELynx.Data.Tree.NamedTree
  ( Named (..)
  ) where

import qualified Data.ByteString.Lazy.Char8 as L

-- | Data types with names.
class Named a where
  getName :: a -> L.ByteString
