{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Description :  Analyze trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May 24 13:47:56 2019.

-}

module Examine.Examine
  ( examine
  )
where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8     as L
import qualified Data.Text                      as T
import           Data.Tree

import           Examine.Options

import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Logger

readTrees :: Maybe FilePath -> Examine [Tree (PhyloLabel L.ByteString)]
readTrees mfp = do
  case mfp of
    Nothing -> $(logInfo) "Read tree(s) from standard input."
    Just fp -> $(logInfo) $ T.pack $ "Read tree(s) from file " <> fp <> "."
  liftIO $ parseFileOrIOWith manyNewick mfp

-- | Examine phylogenetic trees.
examine :: Maybe FilePath -> Examine ()
examine outFn = do
  ExamineArguments inFn <- lift ask
  trs <- readTrees inFn
  let lsStrs = map summarize trs
  let outFilePath = (++ ".out") <$> outFn
  logNewSection "Results."
  io "results of tree analysis" (L.intercalate (L.pack "\n") lsStrs) outFilePath
