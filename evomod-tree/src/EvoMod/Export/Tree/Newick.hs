{- |
Module      :  EvoMod.Export.Tree.Newick
Description :  Export tree objects to Newick format
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 17 13:51:47 2019.

Parts of the code are from https://hackage.haskell.org/package/BiobaseNewick.

See nomenclature in 'EvoMod.Data.Tree.Tree'.

-}

module EvoMod.Export.Tree.Newick
  ( toNewick
  -- , toNewickPhyloIntTree
  -- , toNewickPhyloByteStringTree
  ) where

import qualified Data.ByteString.Lazy.Builder       as L
import qualified Data.ByteString.Lazy.Char8         as L
import           Data.List                          (intersperse)
-- import           Data.Maybe
import           Data.Tree

import           EvoMod.Data.Tree.BranchSupportTree
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.NamedTree
-- import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Tools.ByteString            (c2w)

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
                       <> L.word8 (c2w ')') <> brSup l
                       <> lbl l
    lbl l = L.lazyByteString (name l)
            <> L.word8 (c2w ':')
            <> L.doubleDec (getLen l)
    brSup l = maybe mempty L.doubleDec (getBranchSupport l)

-- -- | Convenience function for exporting trees with 'Int' labels and 'Double'
-- -- branch lengths.
-- toNewickPhyloIntTree :: Tree PhyloIntLabel -> L.ByteString
-- toNewickPhyloIntTree = toNewickWith (L.intDec . pLabel) (L.doubleDec . pBrLen)

-- -- | Convenience function for exporting trees with 'L.ByteString' labels and
-- -- 'Double' branch lengths.
-- toNewickPhyloByteStringTree :: Tree PhyloByteStringLabel -> L.ByteString
-- toNewickPhyloByteStringTree = toNewickWith (L.lazyByteString . pLabel) (L.doubleDec . pBrLen)
