{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Export.Tree.Nexus
-- Description :  Export trees to Nexus files
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 20:24:19 2020.
module ELynx.Export.Tree.Nexus
  ( toNexusTrees,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Tree
import ELynx.Data.Tree
import ELynx.Export.Nexus
import ELynx.Export.Tree.Newick

-- | Export a list of (NAME, TREE) to a Nexus file.
toNexusTrees :: Named a => [(ByteString, Tree (PhyloLabel a))] -> ByteString
toNexusTrees ts = toNexus "TREES" (map tree ts)

tree :: Named a => (ByteString, Tree (PhyloLabel a)) -> ByteString
tree (n, t) = "  TREE " <> n <> " = " <> toNewick t
