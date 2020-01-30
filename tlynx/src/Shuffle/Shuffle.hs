{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Shuffle.Shuffle
Description :  Shuffle a phylogeny.
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep 19 15:01:52 2019.

The coalescent times are unaffected. The topology and the leaf order is
shuffled. Branch support values are ignored and lost.

-}

module Shuffle.Shuffle
  ( shuffleCmd
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Random
import           Control.Monad.Trans.Reader
import           Data.Array                     (elems)
import           Data.Array.ST                  (newListArray, readArray,
                                                 runSTArray, writeArray)
import qualified Data.ByteString.Lazy.Char8     as L
import           Data.List                      (filter)
import           Data.Maybe                     (isNothing)
import           Data.Tree                      (Tree (Node), flatten,
                                                 rootLabel)
import           System.IO

import           Shuffle.Options

import           ELynx.Data.Tree.MeasurableTree (distancesOriginLeaves, height,
                                                 rootHeight)
import           ELynx.Data.Tree.NamedTree      (getName)
import           ELynx.Data.Tree.PhyloTree      (PhyloLabel, brLen)
import           ELynx.Data.Tree.Tree           (leaves)
import           ELynx.Export.Tree.Newick       (toNewick)
import           ELynx.Import.Tree.Newick       (oneNewick)
import           ELynx.Simulate.PointProcess    (PointProcess (PointProcess),
                                                 toReconstructedTree)
import           ELynx.Tools.Definitions        (eps)
import           ELynx.Tools.InputOutput        (outHandle, parseFileWith)
import           ELynx.Tools.Text               (fromBs, tShow)

-- | Shuffle a tree. Get all coalescent times, shuffle them. Get all leaves,
-- shuffle them. Connect the shuffled leaves with the shuffled coalescent times.
-- The shuffled tree has a new topology while keeping the same set of coalescent
-- times and leaves.
shuffleCmd :: Maybe FilePath -> Shuffle ()
shuffleCmd outFile = do
  a <- lift ask

  let outFn = (++ ".tree") <$> outFile
  h <- outHandle "results" outFn

  t <- liftIO $ parseFileWith oneNewick (inFile a)
  $(logInfo) "Input tree:"
  $(logInfo) $ fromBs $ toNewick t

  -- Check if all branches have a given length. However, the length of the stem is not important.
  let r = rootLabel t
      r' = r {brLen = Just 0}
      t' = t {rootLabel = r'}
  when (isNothing $ traverse brLen t')
    (do
        $(logDebug) $ tShow t'
        error "Not all branches have a given length.")

  -- Check if tree is ultrametric enough.
  let dh = sum $ map (height t -) (distancesOriginLeaves t)
  $(logDebug) $ "Distance in branch length to being ultrametric: " <> tShow dh
  when (dh > 2e-4)
    (error "Tree is not ultrametric.")
  when (dh > eps && dh < 2e-4) $
    $(logInfo) "Tree is nearly ultrametric, ignore branch length differences smaller than 2e-4."
  when (dh < eps) $
    $(logInfo) "Tree is ultrametric."

  let cs = filter (>0) $ flatten $ mapTree rootHeight t
      ls = map getName $ leaves t
  $(logDebug) $ "Number of coalescent times: " <> tShow (length cs)
  $(logDebug) $ "Number of leaves: " <> tShow (length ls)
  $(logDebug) "The coalescent times are: "
  $(logDebug) $ tShow cs

  ts <- liftIO $ shuffle (nReplicates a) (height t) cs ls
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts

  liftIO $ hClose h

shuffle :: MonadRandom m
        => Int            -- How many?
        -> Double         -- Stem length.
        -> [Double]       -- Coalescent times.
        -> [L.ByteString] -- Leave names.
        -> m [Tree (PhyloLabel L.ByteString)]
shuffle n o cs ls = do
  css <- grabble cs n (length cs)
  lss <- grabble ls n (length ls)
  return [toReconstructedTree "" (PointProcess names times o) | (times, names) <- zip css lss]

-- TODO: This seems to be 'extend' from comonad!
mapTree :: (Tree a -> b) -> Tree a -> Tree b
mapTree f t@(Node _ cs) = Node (f t) (map (mapTree f) cs)

-- | From https://wiki.haskell.org/Random_shuffle.
--
-- @grabble xs m n@ is /O(m*n')/, where @n' = min n (length xs)@. Choose @n@
-- elements from @xs@, without replacement, and that @m@ times.
grabble :: MonadRandom m => [a] -> Int -> Int -> m [[a]]
grabble xs m n = do
    swapss <- replicateM m $ forM [0 .. min (maxIx - 1) n] $ \i -> do
                j <- getRandomR (i, maxIx)
                return (i, j)
    return $ map (take n . swapElems xs) swapss
    where
        maxIx   = length xs - 1

-- grabbleOnce :: MonadRandom m => [a] -> Int -> m [a]
-- grabbleOnce xs n = head `liftM` grabble xs 1 n

swapElems  :: [a] -> [(Int, Int)] -> [a]
swapElems xs swaps = elems $ runSTArray (do
    arr <- newListArray (0, maxIx) xs
    mapM_ (swap arr) swaps
    return arr)
    where
        maxIx   = length xs - 1
        swap arr (i,j) = do
            vi <- readArray arr i
            vj <- readArray arr j
            writeArray arr i vj
            writeArray arr j vi
