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

import qualified Data.ByteString.Lazy.Char8 as BL

-- XXX: This has to be refined. Like this, only one block can be parsed, and the
-- block type has to be known beforehand.

-- | Create nexus file with block name and block body.
toNexus :: BL.ByteString -> [BL.ByteString] -> BL.ByteString
toNexus n b = BL.unlines $ [start, begin n] <> b <> [end]

start :: BL.ByteString
start = "#NEXUS"

begin :: BL.ByteString -> BL.ByteString
begin n = "BEGIN " <> n <> ";"

end :: BL.ByteString
end = "END;"
