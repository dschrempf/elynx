{-# LANGUAGE OverloadedStrings #-}

{- |
Description :  Compute distances between trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Wed May 29 18:09:39 2019.

- Robinson-Foulds distance.
- Incompatible splits distance.

-}

import qualified Data.ByteString.Builder    as L
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List
import           Data.Tree
import           System.Environment

import           EvoMod.Data.Tree.Distance
import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Data.Tree.BranchSupportTree
import           EvoMod.Import.Tree.Newick
-- import           EvoMod.Export.Tree.Newick

import           EvoMod.Tools.ByteString    (alignRight, alignLeft)
import           EvoMod.Tools.InputOutput

header :: Int -> L.ByteString
header n = alignLeft (n+2) "Tree 1"
           <> alignLeft (n+2) "Tree 2"
           <> alignRight 20 "Symmetric Distance"

showTriplet :: Int -> [String] -> (Int, Int, Int) -> L.ByteString
showTriplet n args (i, j, d) = i' <> j' <> d'
  where i' = alignLeft  (n+2) $ L.pack (args !! i)
        j' = alignLeft  (n+2) $ L.pack (args !! j)
        d' = alignRight 20    $ L.toLazyByteString (L.intDec d)

distanceMeasure :: (Ord a, Eq a) => Tree a -> Tree a -> Int
-- distanceMeasure = robinsonFouldsDistance
distanceMeasure = incompatibleSplitsDistance

main :: IO ()
main = do
  args <- getArgs
  ts <- mapM (parseFileWith newick) args
  let n   = maximum $ map length args
      tsN = map normalize ts
      tsC = map (collapse 0.95) tsN
      ts' = map removeBrLen tsC
      ds = [ (i, j, distanceMeasure a b)
           | (i:ir, a:ar) <- zip (tails [0..]) (tails ts')
           , (j, b) <- zip ir ar ]
  -- L.putStrLn $ L.unlines $ map toNewick ts
  -- L.putStrLn $ L.unlines $ map toNewick tsN
  -- L.putStrLn $ L.unlines $ map toNewick tsC
  L.putStrLn $ header n
  L.putStr $ L.unlines $ map (showTriplet n args) ds
