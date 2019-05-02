{- |
Module      :  EvoMod.Export.Tree.Newick
Description :  Export tree objects to Newick format.
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
  ( toNewickWith
  , toNewickPhyloIntTree
  , toNewickPhyloByteStringTree
  ) where

import qualified Data.ByteString.Lazy.Builder as L
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.List                    (intersperse)
import           Data.Tree

import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Tools.ByteString      (c2w)

-- | General conversion of a tree into a Newick 'L.Bytestring'. Use provided
-- functions to extract node labels and branch lengths builder objects. See also
-- Biobase.Newick.Export.
toNewickWith :: (a -> L.Builder) -> (a -> L.Builder) -> Tree a -> L.ByteString
toNewickWith labelBuilder branchLengthBuilder t =
  L.toLazyByteString $ go t <> L.word8 (c2w ';')
  where
    go (Node l [])   = lbl l
    go (Node l ts)   = L.word8 (c2w '(')
                       <> mconcat (intersperse (L.word8 $ c2w ',') $ map go ts)
                       <> L.word8 (c2w ')')
                       <> lbl l
    lbl l = labelBuilder l
            <> L.word8 (c2w ':')
            <> branchLengthBuilder l

-- | Convenience function for exporting trees with 'Int' labels and 'Double'
-- branch lengths.
toNewickPhyloIntTree :: Tree PhyloIntLabel -> L.ByteString
toNewickPhyloIntTree = toNewickWith (L.intDec . piLabel) (L.doubleDec . piBrLen)

-- | Convenience function for exporting trees with 'L.ByteString' labels and
-- 'Double' branch lengths.
toNewickPhyloByteStringTree :: Tree PhyloByteStringLabel -> L.ByteString
toNewickPhyloByteStringTree = toNewickWith (L.lazyByteString . pbsLabel) (L.doubleDec . pbsBrLen)
