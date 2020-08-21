{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Description :  Compute distances between trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed May 29 18:09:39 2019.
--
-- - Symmetric (Robinson-Foulds) distance.
-- - Incompatible splits distance.
module TLynx.Distance.Distance
  ( distance,
  )
where

import Control.Monad
  ( unless,
    when,
  )
import Control.Monad.IO.Class
import Control.Monad.Logger
  ( logDebug,
    logInfo,
  )
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (local)
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List hiding (intersect)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vector.Unboxed as V
import ELynx.Data.Tree
import ELynx.Export.Tree.Newick
import ELynx.Import.Tree.Newick
import ELynx.Tools
import Statistics.Sample
import System.IO
import TLynx.Distance.Options
import Text.Printf

median :: Ord a => [a] -> a
median xs = sort xs !! l2 where l2 = length xs `div` 2

pf :: String
pf = "%.3f"

header :: Int -> Int -> DistanceMeasure -> BL.ByteString
header n m d =
  alignLeft (n + 2) "Tree 1" <> alignLeft (n + 2) "Tree 2"
    <> alignRight
      (m + 2)
      (BL.pack $ show d)

showTriplet ::
  (PrintfArg a) => Int -> Int -> [String] -> (Int, Int, a) -> BL.ByteString
showTriplet n m args (i, j, d) = i' <> j' <> d'
  where
    i' = alignLeft (n + 2) $ BL.pack (args !! i)
    j' = alignLeft (n + 2) $ BL.pack (args !! j)
    d' = alignRight (m + 2) $ BL.pack (printf pf d)

-- Compute pairwise distances of a list of input trees. Use given distance
-- measure. Returns a triple, the first two elements are the indices of the
-- compared trees, the third is the distance.
pairwise ::
  -- | Distance function
  (a -> a -> b) ->
  -- | Input trees
  [a] ->
  -- | (index i, index j, distance i j)
  [(Int, Int, b)]
pairwise dist trs =
  [ (i, j, dist x y)
    | (i : is, x : xs) <- zip (tails [0 ..]) (tails trs),
      (j, y) <- zip is xs
  ]

-- -- Compute distances between adjacent pairs of a list of input trees. Use given
-- -- distance measure.
-- adjacent ::
--   -- | Distance function.
--   (a -> a -> b) ->
--   -- | Input values.
--   [a] ->
--   [b]
-- adjacent dist trs = [dist x y | (x, y) <- zip trs (tail trs)]

-- | Compute distance functions between phylogenetic trees.
distance :: ELynx DistanceArguments ()
distance = do
  l <- local <$> ask
  let nwFormat = argsNewickFormat l
  -- Determine output handle (stdout or file).
  outH <- outHandle "results" ".out"
  -- Master tree (in case it is given).
  let mname = argsMasterTreeFile l
  mtree <- case mname of
    Nothing -> return Nothing
    Just f -> do
      $(logInfo) $ T.pack $ "Read master tree from file: " <> f <> "."
      t <- liftIO $ parseFileWith (oneNewick nwFormat) f
      $(logInfo) "Compute distances between all trees and master tree."
      return $ Just t
  let tfps = argsInFiles l
  (trees, names) <- case tfps of
    [] -> error "No tree input files given."
    [tf] -> do
      $(logInfo) "Read trees from single file."
      ts <- liftIO $ parseFileWith (someNewick nwFormat) tf
      $(logInfo) $ tShow (length ts) <> " trees found in file."
      $(logInfo) "Trees are indexed with integers."
      return (ts, map show [0 .. length ts - 1])
    _ -> do
      $(logInfo) "Read trees from files."
      ts <- liftIO $ mapM (parseFileWith (oneNewick nwFormat)) tfps
      $(logInfo) "Trees are named according to their file names."
      return (ts, tfps)
  when (null trees) (error "Not enough trees found in files.")
  when
    (isNothing mtree && length trees == 1)
    (error "Not enough trees found in files.")
  -- when (isNothing mtree) $ $(logInfo)
  --   "Compute pairwise distances between trees from different files."
  $(logDebug) "The trees are:"
  $(logDebug) $ LT.toStrict $ LT.decodeUtf8 $ BL.unlines $ map toNewick trees
  -- Set the distance measure.
  let dist = argsDistance l
  case argsDistance l of
    Symmetric -> $(logInfo) "Use symmetric (Robinson-Foulds) distance."
    IncompatibleSplit val -> do
      $(logInfo) "Use incompatible split distance."
      $(logInfo) $
        T.pack $
          "Collapse nodes with support less than "
            ++ show val
            ++ "."
    BranchScore -> $(logInfo) "Use branch score distance."
  let distanceMeasure' ::
        (Measurable e1, Measurable e2) =>
        Tree e1 BS.ByteString ->
        Tree e2 BS.ByteString ->
        Double
      distanceMeasure' t1 t2 = either error id $ case dist of
        Symmetric -> second fromIntegral $ symmetric t1 t2
        IncompatibleSplit _ -> second fromIntegral $ incompatibleSplits t1 t2
        BranchScore -> branchScore t1 t2
  -- Possibly intersect trees before distance calculation.
  when (argsIntersect l) $
    $(logInfo) "Intersect trees before calculation of distances."
  let distanceMeasure =
        if argsIntersect l
          then
            ( \t1 t2 ->
                let [t1', t2'] = either error id $ intersect [t1, t2]
                 in distanceMeasure' t1' t2'
            )
          else distanceMeasure'
  -- Possibly normalize trees.
  when (argsNormalize l) $
    $(logInfo) "Normalize trees before calculation of distances."
  let normalizeF = if argsNormalize l then normalizeBranchLengths else id
  -- Possibly collapse unsupported nodes.
  let collapseF = case dist of
        -- For the incompatible split distance we have to collapse branches with
        -- support lower than the given value. Before doing so, we normalize the
        -- branch support values.
        IncompatibleSplit val -> collapse val . normalizeBranchSupport
        _ -> id
  -- The trees can be prepared now.
  let trees' :: Forest PhyloExplicit BS.ByteString
      trees' = map (collapseF . normalizeF . either error id . toExplicitTree) trees
  $(logDebug) "The prepared trees are:"
  $(logDebug) $ LT.toStrict $ LT.decodeUtf8 $ BL.unlines $ map (toNewick . toPhyloTree) trees'
  let dsTriplets = case mtree of
        Nothing -> pairwise distanceMeasure trees'
        Just pt ->
          let t = (either error id . toExplicitTree) pt
           in [(0, i, distanceMeasure t t') | (i, t') <- zip [1 ..] trees']
      ds = map (\(_, _, x) -> x) dsTriplets
      dsVec = V.fromList ds
  liftIO $
    hPutStrLn outH $
      "Summary statistics of "
        ++ show dist
        ++ " Distance:"
  liftIO $
    T.hPutStrLn outH $
      T.justifyLeft 10 ' ' "Mean: "
        <> T.pack
          (printf pf (mean dsVec))
  liftIO $
    T.hPutStrLn outH $
      T.justifyLeft 10 ' ' "Median: "
        <> T.pack
          (printf pf (median ds))
  liftIO $
    T.hPutStrLn outH $
      T.justifyLeft 10 ' ' "Variance: "
        <> T.pack
          (printf pf (variance dsVec))
  -- BS.putStrLn $ BS.unlines $ map toNewick ts
  -- BS.putStrLn $ BS.unlines $ map toNewick tsN
  -- BS.putStrLn $ BS.unlines $ map toNewick tsC

  lift $
    unless
      (argsSummaryStatistics l)
      ( do
          let n = maximum $ 6 : map length names
              m = length $ show dist
          lift $ hPutStrLn outH ""
          lift $ BL.hPutStrLn outH $ header n m dist
          case mname of
            Nothing ->
              lift $
                BL.hPutStr outH $
                  BL.unlines
                    (map (showTriplet n m names) dsTriplets)
            Just mn ->
              lift $
                BL.hPutStr outH $
                  BL.unlines
                    (map (showTriplet n m (mn : names)) dsTriplets)
      )
  liftIO $ hClose outH
