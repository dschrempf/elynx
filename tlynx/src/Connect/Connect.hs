{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Connect.Connect
Description :  Connect two phylogenies
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep 19 15:01:52 2019.

-}

module Connect.Connect
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

import           Connect.Options

import           ELynx.Data.Tree.Bipartition    ( bipartition
                                                , compatible
                                                )
import           ELynx.Data.Tree.NamedTree      ( getName )
import           ELynx.Data.Tree.PhyloTree      ( PhyloLabel(PhyloLabel) )
import           ELynx.Data.Tree.Tree           ( clades
                                                , connect
                                                )
import           ELynx.Export.Tree.Newick       ( toNewick )
import           ELynx.Import.Tree.Newick       ( manyNewick
                                                , manyNewickIqTree
                                                , oneNewick
                                                , oneNewickIqTree
                                                )
import           ELynx.Tools.InputOutput        ( outHandle
                                                , parseFileWith
                                                )
import           ELynx.Tools.Reproduction       ( ELynx
                                                , Arguments(..)
                                                )
import           ELynx.Tools.Text               ( fromBs
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
      iq = newickIqTreeFlag lArgs
  case cs of
    Nothing -> connectOnly iq outH l r
    Just c  -> connectAndFilter iq outH c l r

  liftIO $ hClose outH

connectTrees
  :: Tree (PhyloLabel L.ByteString)
  -> Tree (PhyloLabel L.ByteString)
  -> [Tree (PhyloLabel L.ByteString)]
connectTrees = connect (PhyloLabel "" Nothing Nothing)

type Constraint a = S.Set a

compatibleAll :: (Show a, Ord a) => Tree a -> [Constraint a] -> Bool
compatibleAll (Node _ [l, r]) cs =
  all (compatible (bipartition l)) cs && all (compatible (bipartition r)) cs
compatibleAll _ _ = error "Tree is not bifurcating."

compatibleWith
  :: (Show b, Ord b) => (a -> b) -> [Constraint a] -> Tree a -> Bool
compatibleWith f cs t = compatibleAll (fmap f t) (map (S.map f) cs)

parseTrees
  :: Bool
  -> FilePath
  -> FilePath
  -> ELynx
       ConnectArguments
       (Tree (PhyloLabel L.ByteString), Tree (PhyloLabel L.ByteString))
parseTrees iqt l r = do
  let oneNw = if iqt then oneNewickIqTree else oneNewick
  tl <- liftIO $ parseFileWith oneNw l
  tr <- liftIO $ parseFileWith oneNw r
  $(logInfo) "Tree 1:"
  $(logInfo) $ fromBs $ toNewick tl
  $(logInfo) "Tree 2:"
  $(logInfo) $ fromBs $ toNewick tr
  return (tl, tr)

connectOnly
  :: Bool -> Handle -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectOnly iq h l r = do
  (tl, tr) <- parseTrees iq l r
  let ts = connectTrees tl tr
  $(logInfo) $ "Connected trees: " <> tShow (length ts)
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts

connectAndFilter
  :: Bool
  -> Handle
  -> FilePath
  -> FilePath
  -> FilePath
  -> ELynx ConnectArguments ()
connectAndFilter iq h c l r = do
  let manyNw = if iq then manyNewickIqTree else manyNewick
  cts <- liftIO $ parseFileWith manyNw c
  $(logInfo) "Constraints:"
  $(logInfo) $ fromBs $ L.intercalate "\n" $ map toNewick cts
  (tl, tr) <- parseTrees iq l r
  let ts  = connectTrees tl tr
      cs  = concatMap clades cts :: [Constraint (PhyloLabel L.ByteString)]
      -- Only collect trees that are compatible with the constraints.
      ts' = filter (compatibleWith getName cs) ts
  $(logInfo) $ "Connected  trees: " <> tShow (length ts)
  $(logInfo) $ "Compatible trees: " <> tShow (length ts')
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts'
