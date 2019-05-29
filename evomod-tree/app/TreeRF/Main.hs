{-# LANGUAGE OverloadedStrings #-}

{- |
Description :  Compute Robinson-Foulds distance between two trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Wed May 29 18:09:39 2019.

-}

import qualified Data.ByteString.Builder    as L
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List
import           System.Environment


import           EvoMod.Data.Tree.Tree
import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Import.Tree.Newick

import           EvoMod.Tools.ByteString    (alignRight)
import           EvoMod.Tools.InputOutput

header :: L.ByteString
header = alignRight 15 "Tree 1" <> alignRight 15 "Tree 2" <> alignRight 20 "Symmetric Distance"

showTriplet :: [String] -> (Int, Int, Int) -> L.ByteString
showTriplet args (i, j, d) = i' <> j' <> d'
  where i' = alignRight 15 $ L.pack (args !! i)
        j' = alignRight 15 $ L.pack (args !! j)
        d' = alignRight 15 $ L.toLazyByteString (L.intDec d)

main :: IO ()
main = do
  args <- getArgs
  ts <- mapM (parseFileWith newick) args
  let ts' = map removeBrLen ts
      ds = [ (i, j, symmetricDistance a b)
           | (i:ir, a:ar) <- zip (tails [0..]) (tails ts')
           , (j, b) <- zip ir ar ]
  L.putStrLn header
  putStr $ unlines $ map (show . bipartitions) ts'
  L.putStr $ L.unlines $ map (showTriplet args) ds


