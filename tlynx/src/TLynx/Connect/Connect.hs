{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  TLynx.Connect.Connect
-- Description :  Connect two phylogenies
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Sep 19 15:01:52 2019.
module TLynx.Connect.Connect
  ( connectCmd,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Set as S
import Data.Tree
import ELynx.Data.Tree
import ELynx.Export.Tree.Newick (toNewick)
import ELynx.Import.Tree.Newick
  ( oneNewick,
    someNewick,
  )
import ELynx.Tools
  ( Arguments (..),
    ELynx,
    fromBs,
    outHandle,
    parseFileWith,
    tShow,
  )
import System.IO
import TLynx.Connect.Options

-- -- | Connect two trees with a branch in all possible ways.
-- --
-- -- Introduce a branch between two trees. If the trees have @n>2@, and @m>2@
-- -- nodes, respectively, there are (n-2)*(m-2) ways to connect them.
-- --
-- -- A base node label has to be given which will be used wherever the new node is
-- -- introduced.
-- --
-- -- Branch labels are not handled.
-- --
-- -- Return 'Left' if one tree has a non-bifurcating root node.
-- connect :: a -> Tree () a -> Tree () a -> Either String (Forest () a)
-- connect lb l r = do
--   ls <- roots l
--   rs <- roots r
--   return [Node () lb [x, y] | x <- ls, y <- rs]

-- | Connect two trees honoring possible constraints. See 'connect'.
connectCmd :: ELynx ConnectArguments ()
connectCmd = do
  lArgs <- local <$> ask
  outH <- outHandle "results" ".out"
  -- Do we have constraints or not?
  let cs = constraints lArgs
      l = inFileA lArgs
      r = inFileB lArgs
  case cs of
    Nothing -> connectOnly outH l r
    Just c -> connectAndFilter outH c l r
  liftIO $ hClose outH

connectTrees ::
  Tree (PhyloLabel L.ByteString) ->
  Tree (PhyloLabel L.ByteString) ->
  Forest (PhyloLabel L.ByteString)
connectTrees = connect (PhyloLabel "" 0 0)

type Constraint a = S.Set a

-- Get groups induced by multifurcations. Collect the leaves of all trees
-- induced by multifurcations.
multifurcatingGroups :: Tree e a -> [[a]]
multifurcatingGroups (Node _ _ []) = []
multifurcatingGroups (Node _ _ [x]) = multifurcatingGroups x
multifurcatingGroups (Node _ _ [x, y]) = multifurcatingGroups x ++ multifurcatingGroups y
multifurcatingGroups t = leaves t : concatMap multifurcatingGroups (forest t)

compatibleAll :: (Show a, Ord a) => Tree a -> [Constraint a] -> Bool
compatibleAll (Node _ [l, r]) cs =
  all (bpcompatible (bipartition l)) cs && all (bpcompatible (bipartition r)) cs
compatibleAll _ _ = error "Tree is not bifurcating."

compatibleWith ::
  (Show b, Ord b) => (a -> b) -> [Constraint a] -> Tree a -> Bool
compatibleWith f cs t = compatibleAll (fmap f t) (map (S.map f) cs)

parseTrees ::
  FilePath ->
  FilePath ->
  ELynx
    ConnectArguments
    (Tree (PhyloLabel L.ByteString), Tree (PhyloLabel L.ByteString))
parseTrees l r = do
  nwF <- nwFormat . local <$> ask
  tl <- liftIO $ parseFileWith (oneNewick nwF) l
  tr <- liftIO $ parseFileWith (oneNewick nwF) r
  $(logInfo) "Tree 1:"
  $(logInfo) $ fromBs $ toNewick tl
  $(logInfo) "Tree 2:"
  $(logInfo) $ fromBs $ toNewick tr
  return (harden tl, harden tr)

connectOnly :: Handle -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectOnly h l r = do
  (tl, tr) <- parseTrees l r
  let ts = connectTrees tl tr
  $(logInfo) $ "Connected trees: " <> tShow (length ts)
  liftIO $ L.hPutStr h $ L.unlines $ map (toNewick . soften) ts

connectAndFilter ::
  Handle -> FilePath -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectAndFilter h c l r = do
  nwF <- nwFormat . local <$> ask
  cts <- liftIO $ map harden <$> parseFileWith (someNewick nwF) c
  $(logInfo) "Constraints:"
  $(logInfo) $ fromBs $ L.intercalate "\n" $ map (toNewick . soften) cts
  (tl, tr) <- parseTrees l r
  let ts = connectTrees tl tr
      cs = concatMap multifurcatingGroups cts :: [Constraint (PhyloLabel L.ByteString)]
      -- Only collect trees that are compatible with the constraints.
      ts' = filter (compatibleWith getName cs) ts
  $(logInfo) $ "Connected  trees: " <> tShow (length ts)
  $(logInfo) $ "Compatible trees: " <> tShow (length ts')
  liftIO $ L.hPutStr h $ L.unlines $ map (toNewick . soften) ts'
