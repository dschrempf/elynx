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

module Distance.Distance
  ( distance
  )
where

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( logDebug
                                                , logInfo
                                                )
import           Control.Monad.Trans.Class      ( lift )
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.List                      ( length
                                                , sort
                                                )
import           Data.Maybe                     ( isNothing )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT
import           Data.Tree                      ( Tree )
import qualified Data.Vector.Unboxed           as V
import           Statistics.Sample              ( mean
                                                , variance
                                                )
import           System.IO                      ( hClose
                                                , hPutStrLn
                                                )
import           Text.Printf                    ( PrintfArg
                                                , printf
                                                )

import           Distance.Options

import           ELynx.Data.Tree.BranchSupportTree
                                               as B
import           ELynx.Data.Tree.Distance
import qualified ELynx.Data.Tree.MeasurableTree
                                               as M
import           ELynx.Data.Tree.NamedTree      ( getName )
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Data.Tree.Tree           ( intersectWith )
import           ELynx.Export.Tree.Newick
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.ByteString         ( alignLeft
                                                , alignRight
                                                )
import           ELynx.Tools.InputOutput        ( getOutFilePath
                                                , outHandle
                                                , parseFileWith
                                                , parseIOWith
                                                )
import           ELynx.Tools.Logger
import           ELynx.Tools.Options            ( ELynx )

median :: Ord a => [a] -> a
median xs = sort xs !! l2 where l2 = length xs `div` 2

pf :: String
pf = "%.3f"

header :: Int -> DistanceMeasure -> L.ByteString
header n d =
  alignLeft (n + 2) "Tree 1" <> alignLeft (n + 2) "Tree 2" <> alignRight
    20
    (L.pack $ show d)

showTriplet :: (PrintfArg a) => Int -> [String] -> (Int, Int, a) -> L.ByteString
showTriplet n args (i, j, d) = i' <> j' <> d'
 where
  i' = alignLeft (n + 2) $ L.pack (args !! i)
  j' = alignLeft (n + 2) $ L.pack (args !! j)
  d' = alignRight 20 $ L.pack (printf pf d)

-- | Compute distance functions between phylogenetic trees.
distance :: DistanceArguments -> ELynx ()
distance a = do
  let nw = if argsNewickIqTree a then manyNewickIqTree else manyNewick
  -- Determine output handle (stdout or file).
  outF <- getOutFilePath ".out"
  outH <- outHandle "results" outF
  -- Master tree (in case it is given).
  let mname = argsMasterTreeFile a
  mtree <- case mname of
    Nothing -> return Nothing
    Just f  -> do
      $(logInfo) $ T.pack $ "Read master tree from file: " <> f <> "."
      ts <- liftIO $ parseFileWith nw f
      let n = length ts
      when (n > 1) (error "More than one tree found in master file.")
      $(logInfo) "Compute distances between all trees and master tree."
      return $ Just (head ts)
  let tfps = argsInFiles a
  (trees, names) <- if null tfps then error "No tree input files given."
    -- do
    --   ts <- if null tfps
    --     then do
    --       $(logInfo) "Read trees from standard input."
    --       liftIO $ parseIOWith nw
    --     else do
    --       let f = head tfps
    --       $(logInfo) $ T.pack $ "Read trees from file: " <> f <> "."
    --       liftIO $ parseFileWith nw f
    --   let n = length ts
    --   when (n < 1) (error "Not enough trees found in file.")
    --   when (isNothing mtree) $ $(logInfo)
    --     "Compute pairwise distances between trees in the same file."
    --   $(logInfo)
    --     $  T.pack
    --     $  "Trees are numbered from 0 to "
    --     ++ show (n - 1)
    --     ++ "."
    --   return (ts, take n (map show [0 :: Int ..]))
    else do
      $(logInfo) "Read trees from files."
      ts <- liftIO $ mapM (parseFileWith newick) tfps
      when (length ts <= 1) (error "Not enough trees found in files.")
      when (isNothing mtree) $ $(logInfo)
        "Compute pairwise distances between trees from different files."
      $(logInfo) "Trees are named according to their file names."
      return (ts, tfps)
  $(logDebug) "The trees are:"
  $(logDebug) $ LT.toStrict $ LT.decodeUtf8 $ L.unlines $ map toNewick trees
  case outF of
    Nothing -> logNewSection "Write results to standard output."
    Just f  -> logNewSection $ T.pack $ "Write results to file " <> f <> "."

  -- Set the distance measure.
  let dist = argsDistance a
  case argsDistance a of
    Symmetric -> $(logInfo) "Use symmetric (Robinson-Foulds) distance."
    IncompatibleSplit val -> do
      $(logInfo) "Use incompatible split distance."
      $(logInfo)
        $  T.pack
        $  "Collapse nodes with support less than "
        ++ show val
        ++ "."
    BranchScore -> $(logInfo) "Use branch score distance."
  let distanceMeasure'
        :: Tree (PhyloLabel L.ByteString)
        -> Tree (PhyloLabel L.ByteString)
        -> Double
      distanceMeasure' = case dist of
        Symmetric -> \t1 t2 -> fromIntegral $ symmetric t1 t2
        IncompatibleSplit _ ->
          \t1 t2 -> fromIntegral $ incompatibleSplits t1 t2
        BranchScore -> branchScore
  -- Possibly intersect trees before distance calculation.
  when (argsIntersect a)
    $ $(logInfo) "Intersect trees before calculation of distances."
  let distanceMeasure = if argsIntersect a
        then
          (\t1 t2 ->
            let [t1', t2'] = intersectWith getName M.extend [t1, t2]
            in 
              -- traceShow (toNewick t1') $ traceShow (toNewick t2') $
                distanceMeasure' t1' t2'
          )
        else distanceMeasure'

  -- Possibly normalize trees.
  when (argsNormalize a)
    $ $(logInfo) "Normalize trees before calculation of distances."
  let normalizeF = if argsNormalize a then normalize else id

  -- Possibly collapse unsupported nodes.
  let collapseF = case dist of
        -- For the incompatible split distance we have to collapse branches with
        -- support lower than the given value. Before doing so, we normalize the
        -- branch support values.
        IncompatibleSplit val -> collapse val . B.normalize
        _                     -> id

  -- The trees can be prepared now.
  let trees' = map (collapseF . normalizeF) trees

  $(logDebug) "The prepared trees are:"
  $(logDebug) $ LT.toStrict $ LT.decodeUtf8 $ L.unlines $ map toNewick trees'
  let dsTriplets = case mtree of
        Nothing -> pairwise distanceMeasure trees'
        Just t ->
          [ (0, i, distanceMeasure t t') | (i, t') <- zip [1 ..] trees' ]
      ds    = map (\(_, _, x) -> x) dsTriplets
      dsVec = V.fromList ds

  liftIO
    $  hPutStrLn outH
    $  "Summary statistics of "
    ++ show dist
    ++ " Distance:"
  liftIO $ T.hPutStrLn outH $ T.justifyLeft 10 ' ' "Mean: " <> T.pack
    (printf pf (mean dsVec))
  liftIO $ T.hPutStrLn outH $ T.justifyLeft 10 ' ' "Median: " <> T.pack
    (printf pf (median ds))
  liftIO $ T.hPutStrLn outH $ T.justifyLeft 10 ' ' "Variance: " <> T.pack
    (printf pf (variance dsVec))
  -- L.putStrLn $ L.unlines $ map toNewick ts
  -- L.putStrLn $ L.unlines $ map toNewick tsN
  -- L.putStrLn $ L.unlines $ map toNewick tsC

  lift $ unless
    (argsSummaryStatistics a)
    (do
      let n = maximum $ 6 : map length names
      lift $ hPutStrLn outH ""
      lift $ L.hPutStrLn outH $ header n dist
      case mname of
        Nothing -> lift $ L.hPutStr outH $ L.unlines
          (map (showTriplet n names) dsTriplets)
        Just mn -> lift $ L.hPutStr outH $ L.unlines
          (map (showTriplet n (mn : names)) dsTriplets)
    )

  liftIO $ hClose outH
