-- |
-- Module      :  ELynx.Export.Tree.Newick
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
-- See nomenclature in 'ELynx.Data.Tree.Tree'.
module ELynx.Export.Tree.Newick
  ( toNewick,
  )
where

import Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as L
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (intersperse)
import ELynx.Data.Tree.Rooted
import ELynx.Data.Tree.Phylogeny
import ELynx.Data.Tree.Named
import ELynx.Tools

toNewickBuilder :: Named a => Tree Phylo a -> Builder
toNewickBuilder t = go t <> L.word8 (c2w ';')
  where
    go (Node b l []) = lbl b l
    go (Node b l ts) =
      L.word8 (c2w '(')
        <> mconcat (intersperse (L.word8 $ c2w ',') $ map go ts)
        <> L.word8 (c2w ')')
        <> lbl b l
    mBrSupBuilder x = maybe mempty (\bs -> L.word8 (c2w '[') <> L.doubleDec bs <> L.word8 (c2w ']')) (brSup x)
    mBrLenBuilder x = maybe mempty (\bl -> L.word8 (c2w ':') <> L.doubleDec bl) (brLen x)
    lbl x y =
      L.lazyByteString (getName y)
        <> mBrLenBuilder x
        -- After reading several discussion, I go for the "more semantical
        -- form" with branch support values in square brackets.
        <> mBrSupBuilder x

-- | General conversion of a tree into a Newick 'L.Bytestring'. Use provided
-- functions to extract node labels and branch lengths builder objects. See also
-- Biobase.Newick.Export.
toNewick :: Named a => Tree Phylo a -> ByteString
toNewick = L.toLazyByteString . toNewickBuilder
