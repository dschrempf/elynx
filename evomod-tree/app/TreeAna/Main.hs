{- |
Description :  Analyze trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May 24 13:47:56 2019.

-}

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8      as L
import           Data.Tree
import           System.IO

import           OptionsTreeAna

import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Import.Tree.Newick
import           EvoMod.Tools.InputOutput
import           EvoMod.Tools.Logger
import           EvoMod.Tools.Options

data Params = Params { arguments  :: Args
                     , mLogHandle :: Maybe Handle }

instance Logger Params where
  verbosity = argsVerbosity . arguments
  mHandle   = mLogHandle

type Ana = ReaderT Params IO

readTrees :: Maybe FilePath -> Ana [Tree PhyloByteStringLabel]
readTrees mfp = do
  case mfp of
    Nothing -> logS "Read tree(s) from standard input."
    Just fp -> logS $ "Read tree(s) from file " ++ fp ++ "."
  lift $ parseFileOrIOWith manyNewick mfp

work :: Ana ()
work = do
  lift (programHeader "tree-ana: Analyze trees.") >>= logS
  a <- arguments <$> ask
  trs <- readTrees (argsInFilePath a)
  let lsStrs = map summarize trs
  let outFilePath = (++ ".out") <$> argsOutBaseName a
  io (L.unlines lsStrs) outFilePath

main :: IO ()
main = do
  args <- parseArgs
  logger <- setupLogger (argsOutBaseName args)
  runReaderT work (Params args logger)
  closeLogger logger

