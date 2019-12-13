{- |
Module      :  ELynx.Export.Tree.Newick
Description :  Export tree objects to Newick format
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 13:51:47 2019.

Parts of the code are from https://hackage.haskell.org/package/BiobaseNewick.

See nomenclature in 'ELynx.Data.Tree.Tree'.

-}

module ELynx.Export.Tree.Newick
  ( toNewick
  -- , toNewickPhyloIntTree
  -- , toNewickPhyloByteStringTree
  ) where

import qualified Data.ByteString.Lazy.Builder      as L
import qualified Data.ByteString.Lazy.Char8        as L
import           Data.List                         (intersperse)
-- import           Data.Maybe
import           Data.Tree

import           ELynx.Data.Tree.BranchSupportTree
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.NamedTree
-- import           ELynx.Data.Tree.PhyloTree
import           ELynx.Tools.ByteString            (c2w)

-- | General conversion of a tree into a Newick 'L.Bytestring'. Use provided
-- functions to extract node labels and branch lengths builder objects. See also
-- Biobase.Newick.Export.
toNewick :: (Named a, Measurable a, BranchSupportLabel a) => Tree a -> L.ByteString
toNewick t =
  L.toLazyByteString $ go t <> L.word8 (c2w ';')
  where
    go (Node l [])   = lbl l
    go (Node l ts)   = L.word8 (c2w '(')
                       <> mconcat (intersperse (L.word8 $ c2w ',') $ map go ts)
                       <> L.word8 (c2w ')')
                       <> lbl l
    brSup s  = L.word8 (c2w '[') <> L.doubleDec s <> L.word8 (c2w ']')
    mBrSup l = maybe mempty brSup (getBranchSupport l)
    lbl l    = L.lazyByteString (getName l)
               <> L.word8 (c2w ':')
               <> L.doubleDec (getLen l)
               -- After reading several discussion, I go for the "more semantical
               -- form" with branch support values in square brackets.
               <> mBrSup l
