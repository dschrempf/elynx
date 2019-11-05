{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Compare.Compare
Description :  Compare two phylogenies
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep 19 15:01:52 2019.

-}

module Compare.Compare
  ( compareCmd
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy       as L
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Tree
import           System.IO

import           Compare.Options

import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Data.Tree.Tree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.InputOutput

treesOneFile :: FilePath -> Compare (Tree (PhyloLabel L.ByteString), Tree (PhyloLabel L.ByteString))
treesOneFile tf = do
  $(logInfo) $ T.pack $ "Parse file '" ++ tf ++ "'."
  ts <- liftIO $ parseFileWith manyNewick tf
  let n = length ts
  case compare n 2 of
    LT -> error "Not enough trees in file."
    GT -> error "Too many trees in file."
    EQ -> return (head ts, head . tail $ ts)

treesTwoFiles :: FilePath -> FilePath -> Compare (Tree (PhyloLabel L.ByteString), Tree (PhyloLabel L.ByteString))
treesTwoFiles tf1 tf2 = do
  $(logInfo) $ T.pack $ "Parse first tree file '" ++ tf1 ++ "'."
  t1 <- liftIO $ parseFileWith oneNewick tf1
  $(logInfo) $ T.pack $ "Parse second tree file '" ++ tf2 ++ "'."
  t2 <- liftIO $ parseFileWith oneNewick tf2
  return (t1, t2)

-- | More detailed comparison of two trees.
compareCmd :: Maybe FilePath -> Compare ()
compareCmd outFile = do
  a <- lift ask
  -- Determine output handle (stdout or file).
  let outFn = (++ ".out") <$> outFile
  outH <- liftIO $ maybe (pure stdout) (`openFile` WriteMode) outFn

  -- Read input.
  let inFiles = argsInFiles a
      nFiles  = length inFiles
  (t1, t2) <- case nFiles of
    1 -> treesOneFile (head inFiles)
    2 -> treesTwoFiles (head inFiles) (head . tail $ inFiles)
    _ -> error "Need two input files with one tree each or one input file with two trees."

  -- Check input.
  let lvs1  = leaves t1
      lvs2  = leaves t2
      lfns1 = map getName lvs1
      lfns2 = map getName lvs2
      s1    = S.fromList lfns1
      s2    = S.fromList lfns2
  _ <- if s1 == s2
       then liftIO $ hPutStrLn outH "Trees have the same set of leaf names."
       else liftIO $ hPutStrLn outH "Trees do not have the same set of leaf names."

  
  return ()
