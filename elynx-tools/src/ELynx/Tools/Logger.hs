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
  ( ELynx
  , logNewSection
  , eLynxWrapper
  )
where

import           Control.Exception.Lifted       ( bracket )
import           Control.Monad.Base             ( liftBase )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( Loc
                                                , LogLevel
                                                , LogSource
                                                , LoggingT
                                                , MonadLogger
                                                , filterLogger
                                                , logInfo
                                                , runLoggingT
                                                )
import           Control.Monad.Trans.Control    ( MonadBaseControl )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                )
import qualified Data.ByteString.Char8         as B
import           Data.Text                      ( Text
                                                , pack
                                                )
import           System.IO                      ( BufferMode(LineBuffering)
                                                , Handle
                                                , IOMode(AppendMode)
                                                , hClose
                                                , hSetBuffering
                                                , openFile
                                                , stderr
                                                )
import           System.Log.FastLogger          ( LogStr
                                                , fromLogStr
                                                )

import           ELynx.Tools.Options
import           ELynx.Tools.Reproduction

-- | Unified way of creating a new section in the log.
logNewSection :: MonadLogger m => Text -> m ()
logNewSection s = $(logInfo) $ "== " <> s

-- | The 'LoggingT' wrapper for ELynx. Prints a header and a footer, logs to
-- 'stderr' if no file is provided. If a log file is provided, log to the file
-- and to 'stderr'.
eLynxWrapper :: String -> ELynx () -> ReaderT GlobalArguments IO ()
eLynxWrapper header worker = do
  a <- ask
  let lvl     = logLevel a
      logFile = (++ ".log") <$> outFileBaseName a
  runELynxLoggingT lvl logFile $ do
    h <- liftIO $ logHeader header
    $(logInfo) $ pack h
    worker
    f <- liftIO logFooter
    $(logInfo) $ pack f

runELynxLoggingT
  :: (MonadBaseControl IO m, MonadIO m)
  => LogLevel
  -> Maybe FilePath
  -> LoggingT m a
  -> m a
runELynxLoggingT lvl f = case f of
  Nothing -> runELynxStderrLoggingT . filterLogger (\_ l -> l >= lvl)
  Just fn -> runELynxFileLoggingT fn . filterLogger (\_ l -> l >= lvl)

runELynxFileLoggingT :: MonadBaseControl IO m => FilePath -> LoggingT m a -> m a
runELynxFileLoggingT fp logger =
  bracket (liftBase $ openFile fp AppendMode) (liftBase . hClose) $ \h ->
    liftBase (hSetBuffering h LineBuffering)
      >> runLoggingT logger (output2H stderr h)

runELynxStderrLoggingT :: MonadIO m => LoggingT m a -> m a
runELynxStderrLoggingT = (`runLoggingT` output stderr)

output :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
output h _ _ _ msg = B.hPutStrLn h ls where ls = fromLogStr msg

output2H :: Handle -> Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
output2H h1 h2 _ _ _ msg = do
  B.hPutStrLn h1 ls
  B.hPutStrLn h2 ls
  where ls = fromLogStr msg
