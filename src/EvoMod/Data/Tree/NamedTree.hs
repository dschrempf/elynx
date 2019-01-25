{- |
Module      :  EvoMod.Data.Tree.NamedTree
Description :  Trees with named nodes
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 24 20:09:20 2019.

-}

module EvoMod.Data.Tree.NamedTree
  ( NamedLabel (..)
  ) where

import           Data.ByteString.Lazy.Char8

class NamedLabel a where
  name :: a -> ByteString
