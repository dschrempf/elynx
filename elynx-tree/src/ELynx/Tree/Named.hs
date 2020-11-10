{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  ELynx.Tree.Named
-- Description :  Trees with named nodes
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 24 20:09:20 2019.
module ELynx.Tree.Named
  ( Name (..),
    Named (..),
  )
where

import Control.DeepSeq
import Data.Aeson
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Double.Conversion.ByteString as BC
import Data.String

-- | Node name.
--
-- Use lazy byte strings because Newick strings are built using chunks.
newtype Name = Name {fromName :: BL.ByteString}
  deriving (Show, Eq)
  deriving (Ord, Monoid, Semigroup, IsString, NFData) via BL.ByteString

-- XXX: This is pretty lame, but I need those instances. At the moment, I just
-- go via 'String', but this is certainly not the best solution.

instance ToJSON Name where
  toJSON = toJSON . BL.unpack . fromName
  toEncoding = toEncoding . BL.unpack . fromName

instance FromJSON Name where
  parseJSON = fmap (Name . BL.pack) . parseJSON

instance Named Name where
  getName = id

-- | Data types with names.
class Named a where
  getName :: a -> Name

instance Named () where
  getName = const (Name BL.empty)

instance Named Int where
  getName = Name . BB.toLazyByteString . BB.intDec

instance Named Double where
  getName = Name . BL.fromStrict . toShortest

instance Named Char where
  getName = Name . BB.toLazyByteString . BB.char8

instance (Named a) => Named [a] where
  getName = Name . BL.concat . map (fromName . getName)

instance Named BL.ByteString where
  getName = Name

instance Named BS.ByteString where
  getName = Name . BL.fromStrict
