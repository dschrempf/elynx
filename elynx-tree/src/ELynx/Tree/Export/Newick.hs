-- |
-- Module      :  ELynx.Tree.Export.Newick
-- Description :  Export tree objects to Newick format
-- Copyright   :  (c) Dominik Schrempf 2021
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
import ELynx.Tree.Length
import ELynx.Tree.Name
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted
import ELynx.Tree.Support

buildBrLen :: Length -> BB.Builder
buildBrLen bl = BB.char8 ':' <> BB.doubleDec (fromLength bl)

buildBrSup :: Support -> BB.Builder
buildBrSup bs = BB.char8 '[' <> BB.doubleDec (fromSupport bs) <> BB.char8 ']'

-- | See 'toNewick'.
toNewickBuilder :: (HasMaybeLength a, HasMaybeSupport a, HasName a) => Tree a -> BB.Builder
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
      BB.lazyByteString (fromName $ getName y)
        <> mBrLenBuilder x
        -- After reading several discussions, I go for the "more semantical
        -- form" with branch support values in square brackets.
        <> mBrSupBuilder x

-- | General conversion of a tree into a Newick 'BL.Bytestring'. Use provided
-- functions to extract node labels and branch lengths builder objects. See also
-- Biobase.Newick.Export.
--
-- Functions to write key value pairs for nodes are not provided. Those can just
-- be set as node names. For example, the posterior density and the confidence
-- interval of a node can be encoded by setting the node name to:
--
-- @
-- "ACTUALNAME[posterior=-2839.2,age_95%_HPD={4.80804,31.6041}]"
-- @
toNewick :: (HasMaybeLength a, HasMaybeSupport a, HasName a) => Tree a -> BL.ByteString
toNewick = BB.toLazyByteString . toNewickBuilder
