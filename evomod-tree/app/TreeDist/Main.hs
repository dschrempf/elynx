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


import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Builder            as L
import qualified Data.ByteString.Lazy.Char8         as L
-- import           Data.List
import           Data.Tree
import qualified Data.Vector.Unboxed                as V
import           Statistics.Sample
import           System.IO

import           OptionsTreeDist

import           EvoMod.Data.Tree.BranchSupportTree
import           EvoMod.Data.Tree.Distance
import qualified EvoMod.Data.Tree.NamedTree         as T
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
  -- Determine output handle (stdout or file).
  let outFilePath = (++ ".out") <$> argsOutFileBaseName args
  outH <- lift $ maybe (pure stdout) (`openFile` WriteMode) outFilePath
  let tfps = argsInFilePaths args
  (trees, names) <- if length tfps == 1
    then
    do let f = head tfps
       logS $ "Read trees from file: " ++ f ++ "."
       ts <- lift $ parseFileWith manyNewick f
       let n = length ts
       when (n <= 1) (error "Not enough trees found in file.")
       lift $ hPutStrLn outH "Compute pairwise distances between trees in the same file."
       lift $ hPutStrLn outH $ "Trees are numbered from 0 to " ++ show (n-1) ++ "."
       return (ts, take n (map show [0 :: Int ..]))
    else
    do logS "Read trees from files."
       ts <- lift $ mapM (parseFileWith newick) tfps
       when (length ts <= 1) (error "Not enough trees found in files.")
       lift $ hPutStrLn outH "Compute pairwise distances between trees from different files."
       lift $ hPutStrLn outH "Trees are named according to their file names."
       return (ts, tfps)
  case outFilePath of
    Nothing -> logNewSection "Write results to standard output."
    Just f  -> logNewSection $ "Write results to file " ++ f ++ "."
  let n        = maximum $ map length names
      tsN      = map normalize trees
      distance = argsDistance args
  case distance of
    Symmetric -> lift $ hPutStrLn outH "Use symmetric (Robinson-Foulds) distance."
    IncompatibleSplit val -> do
      lift $ hPutStrLn outH "Use incompatible split distance."
      lift $ hPutStrLn outH $ "Collapse nodes with support less than " ++ show val ++ "."
  let distanceMeasure :: Tree PhyloByteStringLabel -> Tree PhyloByteStringLabel -> Int
      distanceMeasure = case distance of
        Symmetric           -> symmetricDistanceWith T.name
        IncompatibleSplit _ -> incompatibleSplitsDistanceWith T.name
  let treesCollapsed = case distance of
        Symmetric             -> trees
        IncompatibleSplit val -> map (collapse val) tsN
  let dsTriplets = computePairwiseDistances distanceMeasure treesCollapsed
      ds = map (\(_, _, x) -> fromIntegral x) dsTriplets :: [Double]
      dsVec = V.fromList ds
  lift $ hPutStrLn outH "Summary statistics of distance:"
  lift $ hPutStrLn outH $ "Mean: " ++ show (mean dsVec)
  lift $ hPutStrLn outH $ "Variance: " ++ show (variance dsVec)
  -- L.putStrLn $ L.unlines $ map toNewick ts
  -- L.putStrLn $ L.unlines $ map toNewick tsN
  -- L.putStrLn $ L.unlines $ map toNewick tsC
  unless (argsSummaryStatistics args) (
    do
      lift $ hPutStrLn outH ""
      lift $ L.hPutStrLn outH $ header n
      lift $ L.hPutStr outH $ L.unlines (map (showTriplet n names) dsTriplets)
    )
  lift $ hClose outH

main :: IO ()
main = do
  args <- parseArgs
  logger <- setupLogger (argsOutFileBaseName args)
  runReaderT worker (Params args logger)
  closeLogger logger
