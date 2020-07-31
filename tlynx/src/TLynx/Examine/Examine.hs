{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Description :  Analyze trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri May 24 13:47:56 2019.
module TLynx.Examine.Examine
  ( examine,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Char8 as L
import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\))
import qualified Data.Text as T
import ELynx.Data.Tree
import ELynx.Import.Tree.Newick
import ELynx.Tools
import System.IO
  ( Handle,
    hPutStrLn,
  )
import TLynx.Examine.Options

readTrees :: FilePath -> ELynx ExamineArguments (Forest Phylo L.ByteString)
readTrees fp = do
  $(logInfo) $ T.pack $ "Read tree(s) from file " <> fp <> "."
  nf <- argsNewickFormat . local <$> ask
  liftIO $ parseFileWith (someNewick nf) fp

examineTree :: Named a => Handle -> Tree Phylo a -> IO ()
examineTree h t = do
  hPutStrLn h $ "Number of leaves: " ++ show (length lvs)
  let l = phyloToLengthTree t
  case l of
    Left _ -> hPutStrLn h "Branch lengths not available."
    Right t' -> L.hPutStrLn h $ summarizeBranchLengths t'
  unless (null dups) $
    hPutStrLn h ""
      >> hPutStrLn
        h
        ("Duplicate leaves: " ++ show dups)
  where
    lvs = map getName $ leaves t
    dups = lvs \\ nubOrd lvs

-- | Examine phylogenetic trees.
examine :: ELynx ExamineArguments ()
examine = do
  l <- local <$> ask
  let inFn = argsInFile l
  trs <- readTrees inFn
  outH <- outHandle "results" ".out"
  liftIO $ mapM_ (examineTree outH) trs
