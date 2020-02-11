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
  ( Named(..)
  )
where

import qualified Data.ByteString.Lazy.Builder  as L
import qualified Data.ByteString.Lazy.Char8    as L

-- | Data types with names.
class Named a where
  getName :: a -> L.ByteString

instance Named Int where
  getName = L.toLazyByteString . L.intDec

instance Named Double where
  getName = L.toLazyByteString . L.doubleDec

instance Named Char where
  getName = L.toLazyByteString . L.char8

instance Named L.ByteString where
  getName = id

