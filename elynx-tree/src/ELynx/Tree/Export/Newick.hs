-- |
-- Module      :  ELynx.Tree.Export.Newick
-- Description :  Export tree objects to Newick format
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 13:51:47 2019.
--
-- Some functions are inspired by
-- [Biobase.Newick.Import](https://hackage.haskell.org/package/BiobaseNewick).
--
-- See nomenclature in 'ELynx.Tree'.
module ELynx.Tree.Export.Newick
  ( toNewick,
    toNewickBuilder,
  )
where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intersperse)
import ELynx.Tree.Named
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted

buildBrSup :: Double -> BB.Builder
buildBrSup bs = BB.char8 '[' <> BB.doubleDec bs <> BB.char8 ']'

buildBrLen :: Double -> BB.Builder
buildBrLen bl = BB.char8 ':' <> BB.doubleDec bl

-- | See 'toNewick'.
toNewickBuilder :: Named a => Tree Phylo a -> BB.Builder
toNewickBuilder t = go t <> BB.char8 ';'
  where
    go (Node b l []) = lbl b l
    go (Node b l ts) =
      BB.char8 '('
        <> mconcat (intersperse (BB.char8 ',') $ map go ts)
        <> BB.char8 ')'
        <> lbl b l
    mBrSupBuilder x = maybe mempty buildBrSup (brSup x)
    mBrLenBuilder x = maybe mempty buildBrLen (brLen x)
    lbl x y =
      BB.lazyByteString (getName y)
        <> mBrLenBuilder x
        -- After reading several discussion, I go for the "more semantical
        -- form" with branch support values in square brackets.
        <> mBrSupBuilder x

-- | General conversion of a tree into a Newick 'BL.Bytestring'. Use provided
-- functions to extract node labels and branch lengths builder objects. See also
-- Biobase.Newick.Export.
toNewick :: Named a => Tree Phylo a -> BL.ByteString
toNewick = BB.toLazyByteString . toNewickBuilder
