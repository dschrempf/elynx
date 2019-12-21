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
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8  as L
import           Data.Tree
import           System.IO

import           Connect.Options

import           ELynx.Data.Tree.Bipartition (bipartition, compatible)
import           ELynx.Data.Tree.NamedTree   (getName)
import           ELynx.Data.Tree.PhyloTree   (PhyloLabel (PhyloLabel))
import           ELynx.Data.Tree.Subset      (Subset, smap)
import           ELynx.Data.Tree.Tree        (clades, connect)
import           ELynx.Export.Tree.Newick    (toNewick)
import           ELynx.Import.Tree.Newick    (manyNewick, oneNewick)
import           ELynx.Tools.InputOutput     (outHandle, parseFileWith)
import           ELynx.Tools.Text            (fromBs, tshow)

-- TODO: Write a proper documentation.

-- | Connect two trees honoring possible constraints.
connectCmd :: Maybe FilePath -> Connect ()
connectCmd outFile = do
  -- Determine output handle (stdout or file).
  a <- lift ask
  let outFn = (++ ".out") <$> outFile
  outH <- outHandle "results" outFn

  -- Do we have constraints or not?
  let cs = constraints a
      l  = inFileA a
      r  = inFileB a
  case cs of
    Nothing -> connectOnly outH l r
    Just c  -> connectAndFilter outH c l r

  liftIO $ hClose outH

connectTrees :: Tree (PhyloLabel L.ByteString)
             -> Tree (PhyloLabel L.ByteString)
             -> [Tree (PhyloLabel L.ByteString)]
connectTrees = connect (PhyloLabel "" Nothing 1.0)

type Constraint a = Subset a

compatibleAll :: (Show a, Ord a) => Tree a -> [Constraint a] -> Bool
compatibleAll (Node _ [l, r]) cs = all (compatible (bipartition l)) cs &&
                                   all (compatible (bipartition r)) cs
compatibleAll _ _ = error "Tree is not bifurcating."

compatibleWith :: (Show b, Ord b) => (a -> b) -> [Constraint a] -> Tree a -> Bool
compatibleWith f cs t = compatibleAll (fmap f t) (map (smap f) cs)

parseTrees :: FilePath -> FilePath
           -> Connect (Tree (PhyloLabel L.ByteString), Tree (PhyloLabel L.ByteString))
parseTrees l r = do
  tl <- liftIO $ parseFileWith oneNewick l
  tr <- liftIO $ parseFileWith oneNewick r
  $(logInfo) "Tree 1:"
  $(logInfo) $ fromBs $ toNewick tl
  $(logInfo) "Tree 2:"
  $(logInfo) $ fromBs $ toNewick tr
  return (tl, tr)

connectOnly :: Handle -> FilePath -> FilePath -> Connect ()
connectOnly h l r = do
  (tl, tr) <- parseTrees l r
  let  ts = connectTrees tl tr
  $(logInfo) $ "Connected trees: " <> tshow (length ts)
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts

connectAndFilter :: Handle -> FilePath -> FilePath -> FilePath -> Connect ()
connectAndFilter h c l r = do
  cts <- liftIO $ parseFileWith manyNewick c
  $(logInfo) "Constraints:"
  $(logInfo) $ fromBs $ L.intercalate "\n" $ map toNewick cts
  (tl, tr) <- parseTrees l r
  let ts = connectTrees tl tr
      cs = concatMap clades cts :: [Constraint (PhyloLabel L.ByteString)]
      -- Only collect trees that are compatible with the constraints.
      ts' = filter (compatibleWith getName cs) ts
  $(logInfo) $ "Connected  trees: " <> tshow (length ts)
  $(logInfo) $ "Compatible trees: " <> tshow (length ts')
  liftIO $ L.hPutStr h $ L.unlines $ map toNewick ts'
