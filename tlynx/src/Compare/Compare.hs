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
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           System.IO

import           Compare.Options

import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.Tree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.InputOutput

compareCmd :: Maybe FilePath -> Compare ()
compareCmd outFile = do
  a <- lift ask

  -- Read input.
  let inFiles = argsInFiles a
  when (length inFiles /= 2) $
    error "Need two input files with one tree each."
  let tf1 = head inFiles
      tf2 = inFiles !! 1
  $(logInfo) $ T.pack $ "Parse first tree file '" ++ tf1 ++ "'."
  t1 <- liftIO $ parseFileWith newick (head inFiles)
  $(logInfo) $ T.pack $ "Parse second tree file '" ++ tf1 ++ "'."
  t2 <- liftIO $ parseFileWith newick (inFiles !! 1)

  -- Check input.
  let lvs1  = leaves t1
      lvs2  = leaves t2
      lfns1 = map getName lvs1
      lfns2 = map getName lvs2
      s1    = S.fromList lfns1
      s2    = S.fromList lfns2
  when (s1 /= s2) $
    error "Trees do not have the same set of leaf names."
  when (S.size s1 /= length lvs1) $
    error "Leaf names are not unique."

  -- TODO.

  -- Determine output handle (stdout or file).
  let outFile = (++ ".out") <$> outFile
  outH <- liftIO $ maybe (pure stdout) (`openFile` WriteMode) outFile
  return ()
