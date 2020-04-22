{- |
Module      :  ELynx.Export.Tree.Newick
Description :  Export tree objects to Newick format
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 13:51:47 2019.

Some functions are inspired by
[Biobase.Newick.Import](https://hackage.haskell.org/package/BiobaseNewick).

See nomenclature in 'ELynx.Data.Tree.Tree'.

-}

module ELynx.Export.Tree.Newick
  ( toNewick
  )
where

import qualified Data.ByteString.Lazy.Builder  as L
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.List                      ( intersperse )
import           Data.Tree

import           ELynx.Data.Tree
import           ELynx.Tools

-- | General conversion of a tree into a Newick 'L.Bytestring'. Use provided
-- functions to extract node labels and branch lengths builder objects. See also
-- Biobase.Newick.Export.
toNewick :: Named a => Tree (PhyloLabel a) -> L.ByteString
toNewick t = L.toLazyByteString $ go t <> L.word8 (c2w ';')
 where
  go (Node l []) = lbl l
  go (Node l ts) =
    L.word8 (c2w '(')
      <> mconcat (intersperse (L.word8 $ c2w ',') $ map go ts)
      <> L.word8 (c2w ')')
      <> lbl l
  brSupStr bs = L.word8 (c2w '[') <> L.doubleDec bs <> L.word8 (c2w ']')
  mBrSup l = maybe mempty brSupStr (brSup l)
  brLenStr bl = L.word8 (c2w ':') <> L.doubleDec bl
  mBrLen l = maybe mempty brLenStr (brLen l)
  lbl l =
    L.lazyByteString (getName l)
      <> mBrLen l
             -- After reading several discussion, I go for the "more semantical
             -- form" with branch support values in square brackets.
      <> mBrSup l
