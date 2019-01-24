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
  ) where

import           Data.List                  (intersperse)

import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Tree

import           EvoMod.Tools               (c2w)

-- -- | Extract node information and branch length information from a phylogenetic tree.
-- class BuilderNode n where
--   labelBuilder        :: n -> B.Builder
--   branchLengthBuilder :: n -> B.Builder

-- -- | General conversion of a tree into a Newick string in form of a text object.
-- -- Use provided functions to convert node labels to node and branch length text
-- -- objects. See also Biobase.Newick.Export.
-- toNewick :: (BuilderNode a) => Tree a -> T.Text
-- toNewick t = T.toStrict $ B.toLazyText $ go t <> B.singleton ';'
--   where
--     go (Node l [])   = lbl l
--     go (Node l ts)   = B.singleton '('
--                        <> mconcat (intersperse (B.singleton ',') $ map go ts)
--                        <> B.singleton ')'
--                        <> lbl l
--     lbl l = labelBuilder l
--             <> B.singleton ':'
--             <> branchLengthBuilder l

-- | General conversion of a tree into a Newick string in form of a text object.
-- Use provided functions to extract node labels and branch lengths text builder
-- objects. See also Biobase.Newick.Export.
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

-- -- | Convert a phylogenetic tree with integral node labels into a Newick text
-- -- object. This function is preferable because it uses the text builder and is
-- -- much faster.
-- toNewickIntegral :: (Integral a, RealFloat b) => Tree a -> T.Text
-- toNewickIntegral t = T.toStrict $ B.toLazyText $ toNewickWithBuilder B.decimal Tools.realFloatBuilder t

-- -- | General conversion of a tree into a Newick string in form of a text object.
-- -- Use provided text builders to convert node states and branches to text
-- -- objects.
-- toNewickWithBuilder :: (a -> B.Builder) -> (b -> B.Builder) -> PhyloTree a b c -> B.Builder
-- toNewickWithBuilder f g t = go t `mappend` B.singleton ';'
--   where
--     go (Node s [])   = lbl s
--     go (Node s ts)   = B.singleton '(' `mappend`
--                          mconcat (intersperse (B.singleton ',') $ map go ts)
--                          `mappend` B.singleton ')' `mappend` lbl s
--     lbl (PhyloLabel s l _) = f s `mappend` B.singleton ':' `mappend` g l
