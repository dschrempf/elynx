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
  , eLynxWrapper
  ) where

import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8       as B
import           Data.Text
-- import qualified Data.Text                   as T
import           System.IO
import           System.Log.FastLogger

import           ELynx.Tools.Options

-- | Unified way of creating a new section in the log.
logNewSection :: MonadLogger m => Text -> m ()
logNewSection s = do
  $(logInfo) ""
  $(logInfo) $ "-- " <> s

-- | The 'LoggingT' wrapper for ELynx. Prints a header and a footer, logs to
-- 'stderr' if no file is provided. If a log file is provided, log to the file
-- and to 'stderr'.
eLynxWrapper :: (MonadBaseControl IO m, MonadIO m)
             => LogLevel -> Maybe FilePath -> String -> LoggingT m () -> m ()
eLynxWrapper lvl logFile headerMsg worker = runELynxLoggingT lvl logFile $
  do
    h <- liftIO $ logHeader headerMsg
    $(logInfo) $ pack h
    worker
    f <- liftIO logFooter
    $(logInfo) $ pack f

runELynxLoggingT :: (MonadBaseControl IO m, MonadIO m)
                 => LogLevel -> Maybe FilePath -> LoggingT m a -> m a
runELynxLoggingT lvl f = case f of
  Nothing -> runELynxStderrLoggingT . filterLogger (\_ l -> l >= lvl)
  Just fn -> runELynxFileLoggingT fn . filterLogger (\_ l -> l >= lvl)

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
