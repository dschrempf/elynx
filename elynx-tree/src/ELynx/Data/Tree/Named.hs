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

import Data.ByteString.Lazy.Builder (char8, doubleDec, intDec, toLazyByteString)
import Data.ByteString.Lazy.Char8 (ByteString)

-- | Data types with names.
class Named a where
  getName :: a -> ByteString

instance Named Int where
  getName = toLazyByteString . intDec

instance Named Double where
  getName = toLazyByteString . doubleDec

instance Named Char where
  getName = toLazyByteString . char8

instance Named ByteString where
  getName = id
