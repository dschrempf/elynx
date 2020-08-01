-- |
-- Module      :  ELynx.Data.Tree.Named
-- Description :  Trees with named nodes
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 24 20:09:20 2019.
module ELynx.Data.Tree.Named
  ( Named (..),
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as L
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Double.Conversion.ByteString

-- | Data types with names.
class Named a where
  getName :: a -> L.ByteString

instance Named () where
  getName = const L.empty

instance Named Int where
  getName = L.toLazyByteString . L.intDec

instance Named Double where
  getName = L.fromStrict . toShortest

instance Named Char where
  getName = L.toLazyByteString . L.char8

instance Named L.ByteString where
  getName = id

instance Named B.ByteString where
  getName = L.fromStrict
