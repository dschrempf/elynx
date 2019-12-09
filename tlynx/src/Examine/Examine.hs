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

import           Control.Monad                  (unless)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8     as L
import           Data.List                      (nub, (\\))
import qualified Data.Text                      as T
import           Data.Tree
import           System.IO                      (Handle, hPutStrLn)

import           Examine.Options

import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Data.Tree.Tree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.InputOutput

readTrees :: Maybe FilePath -> Examine [Tree (PhyloLabel L.ByteString)]
readTrees mfp = do
  case mfp of
    Nothing -> $(logInfo) "Read tree(s) from standard input."
    Just fp -> $(logInfo) $ T.pack $ "Read tree(s) from file " <> fp <> "."
  liftIO $ parseFileOrIOWith manyNewick mfp

examineTree :: (Measurable a, Named a)
            => Handle -> Tree a -> IO ()
examineTree h t = do
  L.hPutStrLn h $ summarize t
  unless (null dups) (hPutStrLn h $ "Duplicate leaves: " ++ show dups)
  where lvs = map getName $ leaves t
        dups = lvs \\ nub lvs

-- | Examine phylogenetic trees.
examine :: Maybe FilePath -> Examine ()
examine outFn = do
  ExamineArguments inFn <- lift ask
  trs <- readTrees inFn
  let outFilePath = (++ ".out") <$> outFn
  outH <- outHandle "results" outFilePath
  liftIO $ mapM_ (examineTree outH) trs
