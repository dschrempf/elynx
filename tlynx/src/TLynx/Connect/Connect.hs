{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  TLynx.Connect.Connect
Description :  Connect two phylogenies
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep 19 15:01:52 2019.

-}

module TLynx.Connect.Connect
  ( connectCmd
  )
where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader     ( ask )
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.Set                      as S
import           Data.Tree
import           System.IO

import           TLynx.Connect.Options

import           ELynx.Data.Tree
import           ELynx.Export.Tree.Newick       ( toNewick )
import           ELynx.Import.Tree.Newick       ( manyNewick
                                                , oneNewick
                                                )
import           ELynx.Tools                    ( outHandle
                                                , parseFileWith
                                                , ELynx
                                                , Arguments(..)
                                                , fromBs
                                                , tShow
                                                )

-- | Connect two trees honoring possible constraints. See 'connect'.
connectCmd :: ELynx ConnectArguments ()
connectCmd = do
  lArgs <- local <$> ask
  outH  <- outHandle "results" ".out"

  -- Do we have constraints or not?
  let cs = constraints lArgs
      l  = inFileA lArgs
      r  = inFileB lArgs
  case cs of
    Nothing -> connectOnly outH l r
    Just c  -> connectAndFilter outH c l r

  liftIO $ hClose outH

connectTrees
  :: Tree (PhyloLabel L.ByteString)
  -> Tree (PhyloLabel L.ByteString)
  -> [Tree (PhyloLabel L.ByteString)]
connectTrees = connect (PhyloLabel "" Nothing Nothing)

type Constraint a = S.Set a

compatibleAll :: (Show a, Ord a) => Tree a -> [Constraint a] -> Bool
compatibleAll (Node _ [l, r]) cs =
  all (bpcompatible (bipartition l)) cs && all (bpcompatible (bipartition r)) cs
compatibleAll _ _ = error "Tree is not bifurcating."

compatibleWith
  :: (Show b, Ord b) => (a -> b) -> [Constraint a] -> Tree a -> Bool
compatibleWith f cs t = compatibleAll (fmap f t) (map (S.map f) cs)

parseTrees
  :: FilePath
  -> FilePath
  -> ELynx
       ConnectArguments
       (Tree (PhyloLabel L.ByteString), Tree (PhyloLabel L.ByteString))
parseTrees l r = do
  nwF <- nwFormat . local <$> ask
  tl  <- liftIO $ parseFileWith (oneNewick nwF) l
  tr  <- liftIO $ parseFileWith (oneNewick nwF) r
  $(logInfo) "Tree 1:"
  $(logInfo) $ fromBs $ toNewick tl
  $(logInfo) "Tree 2:"
  $(logInfo) $ fromBs $ toNewick tr
  return (tl, tr)

connectOnly :: Handle -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectOnly h l r = do
  (tl, tr) <- parseTrees l r
  let ts = connectTrees tl tr
  $(logInfo) $ "Connected trees: " <> tShow (length ts)
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts

connectAndFilter
  :: Handle -> FilePath -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectAndFilter h c l r = do
  nwF <- nwFormat . local <$> ask
  cts <- liftIO $ parseFileWith (manyNewick nwF) c
  $(logInfo) "Constraints:"
  $(logInfo) $ fromBs $ L.intercalate "\n" $ map toNewick cts
  (tl, tr) <- parseTrees l r
  let ts  = connectTrees tl tr
      cs  = concatMap clades cts :: [Constraint (PhyloLabel L.ByteString)]
      -- Only collect trees that are compatible with the constraints.
      ts' = filter (compatibleWith getName cs) ts
  $(logInfo) $ "Connected  trees: " <> tShow (length ts)
  $(logInfo) $ "Compatible trees: " <> tShow (length ts')
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts'
