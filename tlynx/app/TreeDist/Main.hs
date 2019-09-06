{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Builder           as L
import qualified Data.ByteString.Lazy.Char8        as L
-- import           Data.List
import           Data.Tree
import qualified Data.Vector.Unboxed               as V
import           Statistics.Sample
import           System.IO

import           OptionsTreeDist

import           ELynx.Data.Tree.BranchSupportTree
import           ELynx.Data.Tree.Distance
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Import.Tree.Newick
-- import           ELynx.Export.Tree.Newick
import           ELynx.Tools.ByteString            (alignLeft, alignRight)
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Logger
import           ELynx.Tools.Options

header :: Int -> L.ByteString
header n = alignLeft (n+2) "Tree 1"
           <> alignLeft (n+2) "Tree 2"
           <> alignRight 20 "Symmetric Distance"

showTriplet :: Int -> [String] -> (Int, Int, Int) -> L.ByteString
showTriplet n args (i, j, d) = i' <> j' <> d'
  where i' = alignLeft  (n+2) $ L.pack (args !! i)
        j' = alignLeft  (n+2) $ L.pack (args !! j)
        d' = alignRight 20    $ L.toLazyByteString (L.intDec d)

type Dist = LoggingT (ReaderT Arguments IO)

work :: Dist ()
work = do
  h <- liftIO $ programHeader "tree-dist: Calculate distances between trees."
  $(logInfoSH) h
  Arguments g c <- lift ask
  -- Determine output handle (stdout or file).
  let outFilePath = (++ ".out") <$> outFileBaseName g
  outH <- liftIO $ maybe (pure stdout) (`openFile` WriteMode) outFilePath
  let tfps = argsInFiles c
  (trees, names) <-
    if length tfps <= 1
    then
      do ts <- if null tfps
              then do $(logInfo) "Read trees from standard input."
                      liftIO $ parseIOWith manyNewick
              else do let f = head tfps
                      $(logInfoSH) $ "Read trees from file: " <> f <> "."
                      liftIO $ parseFileWith manyNewick f
         let n = length ts
         when (n <= 1) (error "Not enough trees found in file.")
         liftIO $ hPutStrLn outH "Compute pairwise distances between trees in the same file."
         liftIO $ hPutStrLn outH $ "Trees are numbered from 0 to " ++ show (n-1) ++ "."
         return (ts, take n (map show [0 :: Int ..]))
    else
      do $(logInfo) "Read trees from files."
         ts <- liftIO $ mapM (parseFileWith newick) tfps
         when (length ts <= 1) (error "Not enough trees found in files.")
         liftIO $ hPutStrLn outH "Compute pairwise distances between trees from different files."
         liftIO $ hPutStrLn outH "Trees are named according to their file names."
         return (ts, tfps)
  case outFilePath of
    Nothing -> logNewSection "Write results to standard output."
    Just f  -> logNewSection $ "Write results to file " <> f <> "."
  let n        = maximum $ map length names
      tsN      = map normalize trees
      distance = argsDistance c
  case distance of
    Symmetric -> liftIO $ hPutStrLn outH "Use symmetric (Robinson-Foulds) distance."
    IncompatibleSplit val -> do
      liftIO $ hPutStrLn outH "Use incompatible split distance."
      liftIO $ hPutStrLn outH $ "Collapse nodes with support less than " ++ show val ++ "."
  let distanceMeasure :: Tree PhyloByteStringLabel -> Tree PhyloByteStringLabel -> Int
      distanceMeasure = case distance of
        Symmetric           -> symmetricDistanceWith getName
        IncompatibleSplit _ -> incompatibleSplitsDistanceWith getName
  let treesCollapsed = case distance of
        Symmetric             -> trees
        IncompatibleSplit val -> map (collapse val) tsN
  let dsTriplets = computePairwiseDistances distanceMeasure treesCollapsed
      ds = map (\(_, _, x) -> fromIntegral x) dsTriplets :: [Double]
      dsVec = V.fromList ds
  liftIO $ hPutStrLn outH "Summary statistics of distance:"
  liftIO $ hPutStrLn outH $ "Mean: " ++ show (mean dsVec)
  liftIO $ hPutStrLn outH $ "Variance: " ++ show (variance dsVec)
  -- L.putStrLn $ L.unlines $ map toNewick ts
  -- L.putStrLn $ L.unlines $ map toNewick tsN
  -- L.putStrLn $ L.unlines $ map toNewick tsC
  lift $ unless (argsSummaryStatistics c) (
    do
      lift $ hPutStrLn outH ""
      lift $ L.hPutStrLn outH $ header n
      lift $ L.hPutStr outH $ L.unlines (map (showTriplet n names) dsTriplets)
    )
  liftIO $ hClose outH

main :: IO ()
main = do
  a <- parseArguments
  let f = outFileBaseName $ globalArgs a
      l = case f of
        Nothing -> runStderrLoggingT work
        Just fn -> runFileLoggingT fn work
  runReaderT l a
