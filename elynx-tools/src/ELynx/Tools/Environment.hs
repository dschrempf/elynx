-- |
-- Module      :  ELynx.Tools.Environment
-- Description :  Runtime environment
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  2 22:46:02 2021.
module ELynx.Tools.Environment
  ( Environment (..),
  )
where

import Control.Concurrent.MVar
import Data.Time
import ELynx.Tools.ExecutionMode
import ELynx.Tools.Logger
import System.IO

-- | The environment of an ELynx run.
data Environment s = Environment
  { settings :: s,
    -- | List will be empty if using 'Quiet'. If 'LogStdOutAndFile' is used
    -- 'logHandles' contains two handles to the standard output and the log
    -- file.
    logHandles :: [Handle],
    -- | MVar blocking output.
    outLock :: MVar (),
    -- | Used to calculate the ETA.
    startingTime :: UTCTime
  }
  deriving (Eq)

instance HasExecutionMode s => HasExecutionMode (Environment s) where
  getExecutionMode = getExecutionMode . settings

instance HasLock (Environment s) where
  getLock = outLock

instance HasLogHandles (Environment s) where
  getLogHandles = logHandles

instance HasStartingTime (Environment s) where
  getStartingTime = startingTime

instance HasVerbosity s => HasVerbosity (Environment s) where
  getVerbosity = getVerbosity . settings

-- | Initialize the environment.
--
-- Open log file, get current time.
initializeEnvironment ::
  (HasMaybeOutFileBaseName s, HasExecutionMode s, HasVerbosity s) =>
  s ->
  IO (Environment s)
initializeEnvironment s = do
  t <- getCurrentTime
  mh <- case (getLogMode s, getVerbosity s) of
    (_, Quiet) -> return []
    (LogStdOutAndFile, _) -> do
      h <- openWithExecutionMode em fn
      return [stdout, h]
    (LogFileOnly, _) -> do
      h <- openWithExecutionMode em fn
      return [h]
    (LogStdOutOnly, _) -> return [stdout]
  lock <- newMVar ()
  return $ Environment s mh lock t
  where
    fn = fromAnalysisName (getAnalysisName s) ++ ".mcmc.log"
    em = getExecutionMode s

-- | Close file handles.
closeEnvironment :: Environment s -> IO ()
closeEnvironment e = forM_ hs hClose
  where
    hs = filter (/= stdout) $ logHandles e
