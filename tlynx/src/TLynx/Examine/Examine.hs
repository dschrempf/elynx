{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Description :  Analyze trees
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May 24 13:47:56 2019.

-}

module TLynx.Examine.Examine
  ( examine
  )
where

import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader     ( ask )
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.List                      ( nub
                                                , (\\)
                                                )
import qualified Data.Text                     as T
import           Data.Tree
import           System.IO                      ( Handle
                                                , hPutStrLn
                                                )

import           TLynx.Examine.Options

import           ELynx.Data.Tree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools

readTrees
  :: Bool -> FilePath -> ELynx ExamineArguments [Tree (PhyloLabel L.ByteString)]
readTrees iqtree fp = do
  $(logInfo) $ T.pack $ "Read tree(s) from file " <> fp <> "."
  let nw = if iqtree then manyNewickIqTree else manyNewick
  liftIO $ parseFileWith nw fp

examineTree :: (Measurable a, Named a) => Handle -> Tree a -> IO ()
examineTree h t = do
  L.hPutStrLn h $ summarize t
  unless (null dups) (hPutStrLn h $ "Duplicate leaves: " ++ show dups)
 where
  lvs  = map getName $ leaves t
  dups = lvs \\ nub lvs

-- | Examine phylogenetic trees.
examine :: ELynx ExamineArguments ()
examine = do
  l <- local <$> ask
  let inFn   = argsInFile l
      iqtree = argsNewickIqTree l
  trs  <- readTrees iqtree inFn
  outH <- outHandle "results" ".out"
  liftIO $ mapM_ (examineTree outH) trs
