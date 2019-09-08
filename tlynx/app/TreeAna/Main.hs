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

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8     as L
import qualified Data.Text                      as T
import           Data.Tree

import           OptionsTreeAna

import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Logger
import           ELynx.Tools.Options

type Ana = LoggingT (ReaderT Arguments IO)

readTrees :: Maybe FilePath -> Ana [Tree PhyloByteStringLabel]
readTrees mfp = do
  case mfp of
    Nothing -> $(logInfo) "Read tree(s) from standard input."
    Just fp -> $(logInfo) $ T.pack $ "Read tree(s) from file " <> fp <> "."
  liftIO $ parseFileOrIOWith manyNewick mfp

work :: Ana ()
work = do
  h <- liftIO $ logHeader "tree-ana: Analyze trees."
  $(logInfo) $ T.pack h
  Arguments inFn a <- lift ask
  trs <- readTrees inFn
  let lsStrs = map summarize trs
  let outFilePath = (++ ".out") <$> outFileBaseName a
  logNewSection "Results."
  io "results of tree analysis" (L.intercalate (L.pack "\n") lsStrs) outFilePath
  f <- liftIO logFooter
  $(logInfo) $ T.pack f


main :: IO ()
main = do
  a <- parseArguments
  let f = outFileBaseName $ globalArgs a
      l = case f of
        Nothing -> runELynxStderrLoggingT work
        Just fn -> runELynxFileLoggingT fn work
  runReaderT l a

