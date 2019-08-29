{-# LANGUAGE OverloadedStrings #-}

{- |
Description :  Compute distances between trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Wed May 29 18:09:39 2019.

- Symmetric (Robinson-Foulds) distance.
- Incompatible splits distance.

-}


import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Builder            as L
import qualified Data.ByteString.Lazy.Char8         as L
import           Data.List
import           Data.Tree
import           System.IO

import           OptionsTreeDist

import           EvoMod.Data.Tree.BranchSupportTree
import           EvoMod.Data.Tree.Distance
import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Import.Tree.Newick
-- import           EvoMod.Export.Tree.Newick
import           EvoMod.Tools.ByteString            (alignLeft, alignRight)
import           EvoMod.Tools.InputOutput
import           EvoMod.Tools.Logger
import           EvoMod.Tools.Options

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
-- distanceMeasure = symmetricDistance
distanceMeasure = incompatibleSplitsDistance

type Dist = ReaderT Params IO

data Params = Params { arguments  :: Args
                     , mLogHandle :: Maybe Handle }

instance Logger Params where
  verbosity = argsVerbosity . arguments
  mHandle = mLogHandle

worker :: Dist ()
worker = do
  lift (programHeader "tree-dist: Calculate distances between trees.") >>= logS
  args <- arguments <$> ask
  let tfps = argsInFilePaths args
  (trees, names) <- if length tfps == 1
    then
    do let f = head tfps
       logS $ "Read trees from file: " ++ show f ++ "."
       ts <- lift $ parseFileWith manyNewick (head tfps)
       let n = length ts
       return (ts, take n (map show [0 :: Int ..]))
    else
    do logS "Read trees from files."
       ts <- lift $ mapM (parseFileWith newick) tfps
       return (ts, tfps)
  let n   = maximum $ map length names
      tsN = map normalize trees
      tsC = map (collapse 0.95) tsN
      ts' = map removeBrLen tsC
      ds = [ (i, j, distanceMeasure a b)
           | (i:ir, a:ar) <- zip (tails [0..]) (tails ts')
           , (j, b) <- zip ir ar ]
  -- L.putStrLn $ L.unlines $ map toNewick ts
  -- L.putStrLn $ L.unlines $ map toNewick tsN
  -- L.putStrLn $ L.unlines $ map toNewick tsC
  let res = header n <> L.unlines (map (showTriplet n tfps) ds)
      outFilePath = (++ ".out") <$> argsOutFileBaseName args
  io res outFilePath

main :: IO ()
main = do
  args <- parseArgs
  logger <- setupLogger (argsOutFileBaseName args)
  runReaderT worker (Params args logger)
  closeLogger logger
