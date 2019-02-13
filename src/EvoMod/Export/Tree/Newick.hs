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

import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Char8   as B
import           Data.List                    (intersperse)
import           Data.Tree

import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Tools                 (c2w)

-- | General conversion of a tree into a Newick 'B.Bytestring'. Use provided
-- functions to extract node labels and branch lengths builder objects. See also
-- Biobase.Newick.Export.
toNewickWith :: (a -> B.Builder) -> (a -> B.Builder) -> Tree a -> B.ByteString
toNewickWith labelBuilder branchLengthBuilder t =
  B.toLazyByteString $ go t <> B.word8 (c2w ';')
  where
    go (Node l [])   = lbl l
    go (Node l ts)   = B.word8 (c2w '(')
                       <> mconcat (intersperse (B.word8 $ c2w ',') $ map go ts)
                       <> B.word8 (c2w ')')
                       <> lbl l
    lbl l = labelBuilder l
            <> B.word8 (c2w ':')
            <> branchLengthBuilder l

-- | Convenience function for exporting trees with 'Int' labels and 'Double'
-- branch lengths.
toNewickPhyloIntTree :: Tree PhyloIntLabel -> B.ByteString
toNewickPhyloIntTree = toNewickWith (B.intDec . piLabel) (B.doubleDec . piBrLen)

-- | Convenience function for exporting trees with 'ByteString' labels and 'Double'
-- branch lengths.
toNewickPhyloByteStringTree :: Tree PhyloByteStringLabel -> B.ByteString
toNewickPhyloByteStringTree = toNewickWith (B.lazyByteString . pbsLabel) (B.doubleDec . pbsBrLen)
