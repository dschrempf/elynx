{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  ELynx.Tools.Logger
Description :  Monad logger utility functions
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Sep  6 14:43:19 2019.

-}

module ELynx.Tools.Logger
  ( logNewSection
  , runELynxFileLoggingT
  , runELynxStderrLoggingT
  ) where

import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Text
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8       as B
-- import qualified Data.Text                   as T
import           System.IO
import           System.Log.FastLogger

logNewSection :: MonadLogger m => Text -> m ()
logNewSection s = do
  $(logInfo) ""
  $(logInfo) $ "-- " <> s

runELynxFileLoggingT :: MonadBaseControl IO m => FilePath -> LoggingT m a -> m a
runELynxFileLoggingT fp logger = bracket
    (liftBase $ openFile fp AppendMode)
    (liftBase . hClose)
    $ \h -> liftBase (hSetBuffering h LineBuffering) >> runLoggingT logger (output2H stderr h)

runELynxStderrLoggingT :: MonadIO m => LoggingT m a -> m a
runELynxStderrLoggingT = (`runLoggingT` output stderr)

output :: Handle
       -> Loc
       -> LogSource
       -> LogLevel
       -> LogStr
       -> IO ()
output h loc src lvl msg =
  B.hPutStr h ls
  where
    ls = logStrToBS loc src lvl msg

output2H :: Handle
         -> Handle
         -> Loc
         -> LogSource
         -> LogLevel
         -> LogStr
         -> IO ()
output2H h1 h2 loc src lvl msg = do
  B.hPutStr h1 ls
  B.hPutStr h2 ls
  where
    ls = logStrToBS loc src lvl msg

logStrToBS :: Loc
           -> LogSource
           -> LogLevel
           -> LogStr
           -> B.ByteString
-- logStrToBS loc src lvl msg =
--   fromLogStr $ getLogStr loc src lvl msg
logStrToBS _ _ _ msg = fromLogStr msg <> "\n"

-- getLogStr :: Loc
--           -> LogSource
--           -> LogLevel
--           -> LogStr
--           -> LogStr
-- getLogStr _ src level msg =
--     "[" `mappend` logLevelStr level `mappend`
--     (if T.null src
--         then mempty
--         else "#" `mappend` toLogStr src) `mappend`
--     "] " `mappend`
--     msg `mappend` "\n"

-- logLevelStr :: LogLevel -> LogStr
-- logLevelStr level = case level of
--     LevelOther t -> toLogStr t
--     _            -> toLogStr $ B.pack $ drop 5 $ show level
