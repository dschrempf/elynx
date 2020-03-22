{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Shuffle.Shuffle
Description :  Shuffle a phylogeny
Copyright   :  (c) Dominik Schrempf 2020
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
  )
where

import           Control.Comonad                ( extend )
import           Control.Monad                  ( forM
                                                , replicateM
                                                , when
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( logDebug
                                                , logInfo
                                                )
import           Control.Monad.Trans.Reader     ( ask )
import           Control.Monad.Primitive        ( PrimMonad
                                                , PrimState
                                                )
import           Data.Array                     ( elems )
import           Data.Array.ST                  ( newListArray
                                                , readArray
                                                , runSTArray
                                                , writeArray
                                                )
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.List                      ( filter )
import           Data.Maybe                     ( isNothing )
import           Data.Tree                      ( Tree
                                                , flatten
                                                , rootLabel
                                                )
import           System.IO                      ( hClose )
import           System.Random.MWC              ( Gen
                                                , initialize
                                                , uniformR
                                                )

import           Shuffle.Options

import           ELynx.Data.Tree
import           ELynx.Export.Tree.Newick       ( toNewick )
import           ELynx.Import.Tree.Newick       ( oneNewick
                                                , oneNewickIqTree
                                                )
import           ELynx.Simulate.PointProcess    ( PointProcess(PointProcess)
                                                , toReconstructedTree
                                                )
import           ELynx.Tools

-- | Shuffle a tree. Get all coalescent times, shuffle them. Get all leaves,
-- shuffle them. Connect the shuffled leaves with the shuffled coalescent times.
-- The shuffled tree has a new topology while keeping the same set of coalescent
-- times and leaves.
shuffleCmd :: ELynx ShuffleArguments ()
shuffleCmd = do
  l <- local <$> ask
  h <- outHandle "results" ".tree"

  let oneNw = if newickIqTreeFlag l then oneNewickIqTree else oneNewick
  t <- liftIO $ parseFileWith oneNw (inFile l)
  $(logInfo) "Input tree:"
  $(logInfo) $ fromBs $ toNewick t

  -- Check if all branches have a given length. However, the length of the stem is not important.
  let r  = rootLabel t
      r' = r { brLen = Just 0 }
      t' = t { rootLabel = r' }
  when
    (isNothing $ traverse brLen t')
    (do
      $(logDebug) $ tShow t'
      error "Not all branches have a given length."
    )

  -- Check if tree is ultrametric enough.
  let dh = sum $ map (height t -) (distancesOriginLeaves t)
  $(logDebug) $ "Distance in branch length to being ultrametric: " <> tShow dh
  when (dh > 2e-4) (error "Tree is not ultrametric.")
  when (dh > eps && dh < 2e-4)
    $ $(logInfo)
        "Tree is nearly ultrametric, ignore branch length differences smaller than 2e-4."
  when (dh < eps) $ $(logInfo) "Tree is ultrametric."

  let cs = filter (> 0) $ flatten $ extend rootHeight t
      ls = map getName $ leaves t
  $(logDebug) $ "Number of coalescent times: " <> tShow (length cs)
  $(logDebug) $ "Number of leaves: " <> tShow (length ls)
  $(logDebug) "The coalescent times are: "
  $(logDebug) $ tShow cs

  gen <- case argsSeed l of
    Random  -> error "Seed not available; please contact maintainer."
    Fixed s -> liftIO $ initialize s

  ts <- liftIO $ shuffle (nReplicates l) (height t) cs ls gen
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts

  liftIO $ hClose h

shuffle
  :: PrimMonad m
  => Int            -- How many?
  -> Double         -- Stem length.
  -> [Double]       -- Coalescent times.
  -> [L.ByteString] -- Leave names.
  -> Gen (PrimState m)
  -> m [Tree (PhyloLabel L.ByteString)]
shuffle n o cs ls gen = do
  css <- grabble cs n (length cs) gen
  lss <- grabble ls n (length ls) gen
  return
    [ toReconstructedTree "" (PointProcess names times o)
    | (times, names) <- zip css lss
    ]

-- | From https://wiki.haskell.org/Random_shuffle.
--
-- @grabble xs m n@ is /O(m*n')/, where @n' = min n (length xs)@. Choose @n@
-- elements from @xs@, without replacement, and that @m@ times.
grabble :: PrimMonad m => [a] -> Int -> Int -> Gen (PrimState m) -> m [[a]]
grabble xs m n gen = do
  swapss <- replicateM m $ forM [0 .. min (maxIx - 1) n] $ \i -> do
    j <- uniformR (i, maxIx) gen
    return (i, j)
  return $ map (take n . swapElems xs) swapss
  where maxIx = length xs - 1

-- grabbleOnce :: MonadRandom m => [a] -> Int -> m [a]
-- grabbleOnce xs n = head `liftM` grabble xs 1 n

swapElems :: [a] -> [(Int, Int)] -> [a]
swapElems xs swaps = elems $ runSTArray
  (do
    arr <- newListArray (0, maxIx) xs
    mapM_ (swap arr) swaps
    return arr
  )
 where
  maxIx = length xs - 1
  swap arr (i, j) = do
    vi <- readArray arr i
    vj <- readArray arr j
    writeArray arr i vj
    writeArray arr j vi
