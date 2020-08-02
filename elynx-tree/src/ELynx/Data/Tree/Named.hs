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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Double.Conversion.ByteString as BC

-- | Data types with names.
class Named a where
  -- Use lazy byte strings because Newick strings are built using chunks.
  getName :: a -> BL.ByteString

instance Named () where
  getName = const BL.empty

instance Named Int where
  getName = BB.toLazyByteString . BB.intDec

instance Named Double where
  getName = BL.fromStrict . toShortest

instance Named Char where
  getName = BB.toLazyByteString . BB.char8

instance Named BL.ByteString where
  getName = id

instance Named BS.ByteString where
  getName = BL.fromStrict
