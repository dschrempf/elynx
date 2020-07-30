{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Export.Nexus
-- Description :  Nexus types and classes
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 17:10:05 2020.
module ELynx.Export.Nexus
  ( toNexus,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L

-- This has to be refined. Like this, only one block can be parsed, and the
-- block type has to be known beforehand.

-- | Create nexus file with block name and block body.
toNexus :: ByteString -> [ByteString] -> ByteString
toNexus n b = L.unlines $ [start, begin n] <> b <> [end]

start :: ByteString
start = "#NEXUS"

begin :: ByteString -> ByteString
begin n = "BEGIN " <> n <> ";"

end :: ByteString
end = "END;"
