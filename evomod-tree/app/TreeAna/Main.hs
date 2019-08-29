{- |
Description :  Analyze trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May 24 13:47:56 2019.

-}

import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Tree

import           OptionsTreeAna

import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Data.Tree.Tree
import           EvoMod.Import.Tree.Newick
import           EvoMod.Tools.InputOutput

getTree :: L.ByteString -> Tree PhyloByteStringLabel
getTree = parseByteStringWith "Custom newick tree string" newick

getLeaveNames :: L.ByteString -> L.ByteString
getLeaveNames i = L.unwords lsStr
  where t = getTree i
        ls = leaves t
        lsStr = map pLabel ls

main :: IO ()
main = do
  _ <- parseArgs
  L.interact getLeaveNames

