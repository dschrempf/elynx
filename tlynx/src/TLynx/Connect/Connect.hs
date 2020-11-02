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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import ELynx.Tools
  ( Arguments (..),
    ELynx,
    fromBs,
    outHandle,
    tShow,
  )
import ELynx.Tree
import System.IO
import TLynx.Connect.Options
import TLynx.Parsers

-- Connect two trees with a branch in all possible ways.
--
-- Introduce a branch between two trees. If the trees have @n>2@, and @m>2@
-- nodes, respectively, there are (n-2)*(m-2) ways to connect them.
--
-- A base node label has to be given which will be used wherever the new node is
-- introduced.
--
-- Return 'Left' if one tree has a non-bifurcating root node.
connect :: (Semigroup e, Splittable e) => e -> a -> Tree e a -> Tree e a -> Either String (Forest e a)
connect br lb l r = do
  ls <- roots l
  rs <- roots r
  return [Node br lb [x, y] | x <- ls, y <- rs]

-- | Connect two trees honoring possible constraints.
--
-- Introduce a branch between two trees. If the trees have @n>2@, and @m>2@
-- nodes, respectively, there are (n-2)*(m-2) ways to connect them.
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
  Tree BranchLength BS.ByteString ->
  Tree BranchLength BS.ByteString ->
  Forest BranchLength BS.ByteString
connectTrees t = either error id . connect 0 "root" t

type Constraint a = S.Set a

-- Get groups induced by multifurcations. Collect the leaves of all trees
-- induced by multifurcations.
multifurcatingGroups :: Tree e a -> [[a]]
multifurcatingGroups (Node _ _ []) = []
multifurcatingGroups (Node _ _ [x]) = multifurcatingGroups x
multifurcatingGroups (Node _ _ [x, y]) = multifurcatingGroups x ++ multifurcatingGroups y
multifurcatingGroups t = leaves t : concatMap multifurcatingGroups (forest t)

compatibleAll :: (Show a, Ord a) => Tree e a -> [Constraint a] -> Bool
compatibleAll t@(Node _ _ [l, r]) cs =
  all (compatible partitionLeft . getP) cs
    && all (compatible partitionRight . getP) cs
  where
    lvs = S.fromList $ leaves t
    getP x = either error id $ pt [x, lvs S.\\ x]
    partitionLeft = either error id $ partition l
    partitionRight = either error id $ partition r
compatibleAll _ _ = error "Tree is not bifurcating."

compatibleWith ::
  (Show b, Ord b) => (a -> b) -> [Constraint a] -> Tree e a -> Bool
compatibleWith f cs t = compatibleAll (fmap f t) (map (S.map f) cs)

parseTreeTuple ::
  FilePath ->
  FilePath ->
  ELynx
    ConnectArguments
    (Tree BranchLength BS.ByteString, Tree BranchLength BS.ByteString)
parseTreeTuple l r = do
  nwF <- nwFormat . local <$> ask
  tl <- liftIO $ parseTree nwF l
  tr <- liftIO $ parseTree nwF r
  $(logInfo) "Tree 1:"
  $(logInfo) $ fromBs $ toNewick tl
  $(logInfo) "Tree 2:"
  $(logInfo) $ fromBs $ toNewick tr
  return (either error id $ phyloToLengthTree tl, either error id $ phyloToLengthTree tr)

connectOnly :: Handle -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectOnly h l r = do
  (tl, tr) <- parseTreeTuple l r
  let ts = connectTrees tl tr
  $(logInfo) $ "Connected trees: " <> tShow (length ts)
  liftIO $ BL.hPutStr h $ BL.unlines $ map (toNewick . measurableToPhyloTree) ts

connectAndFilter ::
  Handle -> FilePath -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectAndFilter h c l r = do
  nwF <- nwFormat . local <$> ask
  cts <- liftIO $ parseTrees nwF c
  $(logInfo) "Constraints:"
  $(logInfo) $ fromBs $ BL.intercalate "\n" $ map toNewick cts
  (tl, tr) <- parseTreeTuple l r
  let ts = connectTrees tl tr
      cs = map S.fromList $ concatMap multifurcatingGroups cts :: [Constraint BS.ByteString]
      -- Only collect trees that are compatible with the constraints.
      ts' = filter (compatibleWith getName cs) ts
  $(logInfo) $ "Connected  trees: " <> tShow (length ts)
  $(logInfo) $ "Compatible trees: " <> tShow (length ts')
  liftIO $ BL.hPutStr h $ BL.unlines $ map (toNewick . measurableToPhyloTree) ts'
