{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  ELynx.Tools.Logger
Description :  Monad logger utility functions
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Sep  6 14:43:19 2019.

-}

module ELynx.Tools.Logger
  ( -- * Logger
    logNewSection
  , eLynxWrapper
  )
where

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
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT) )
import qualified Data.ByteString.Char8         as B
import           Data.Text                      ( Text
                                                , pack
                                                )
import           System.IO                      ( BufferMode(LineBuffering)
                                                , Handle
                                                , IOMode(WriteMode)
                                                , hClose
                                                , hSetBuffering
                                                , stderr
                                                )
import           System.Log.FastLogger          ( LogStr
                                                , fromLogStr
                                                )
import           System.Random.MWC              ( createSystemRandom
                                                , save
                                                , fromSeed
                                                )

import           ELynx.Tools.Reproduction       ( Reproducible(..)
                                                , writeReproduction
                                                , ToJSON
                                                , toLogLevel
                                                , ELynx
                                                , Arguments(..)
                                                , Force
                                                , GlobalArguments(..)
                                                , Seed(..)
                                                , logHeader
                                                , logFooter
                                                )
import           ELynx.Tools.InputOutput        ( openFile' )

-- | Unified way of creating a new section in the log.
logNewSection :: MonadLogger m => Text -> m ()
logNewSection s = $(logInfo) $ "== " <> s

-- | The 'ReaderT' and 'LoggingT' wrapper for ELynx. Prints a header and a
-- footer, logs to 'stderr' if no file is provided. Initializes the seed if none
-- is provided. If a log file is provided, log to the file and to 'stderr'.
eLynxWrapper
  :: forall a b
   . (Eq a, Show a, Reproducible a, ToJSON a)
  => Arguments a
  -> (Arguments a -> Arguments b)
  -> ELynx b ()
  -> IO ()
eLynxWrapper args f worker = do
  -- Arguments.
  let gArgs = global args
      lArgs = local args
  let lvl     = toLogLevel $ verbosity gArgs
      rd      = forceReanalysis gArgs
      outBn   = outFileBaseName gArgs
      logFile = (++ ".log") <$> outBn
  runELynxLoggingT lvl rd logFile $ do
    -- Header.
    h <- liftIO $ logHeader (cmdName @a) (cmdDsc @a)
    $(logInfo) $ pack $ h ++ "\n"
    -- Fix seed.
    lArgs' <- case getSeed lArgs of
      Nothing     -> return lArgs
      Just Random -> do
        -- XXX: Have to go via a generator here, since creation of seed is not
        -- supported.
        g <- liftIO createSystemRandom
        s <- liftIO $ fromSeed <$> save g
        $(logInfo) $ pack $ "Seed: random; set to " <> show s <> "."
        return $ setSeed lArgs s
      Just (Fixed s) -> do
        $(logInfo) $ pack $ "Seed: " <> show s <> "."
        return lArgs
    let args' = Arguments gArgs lArgs'
    -- Run the worker with the fixed seed.
    runReaderT worker $ f args'
    -- Reproduction file.
    case outBn of
      Nothing ->
        $(logInfo)
          "No output file given --- skip writing ELynx file for reproducible runs."
      Just bn -> do
        $(logInfo) "Write ELynx reproduction file."
        liftIO $ writeReproduction bn args'
    -- Footer.
    ftr <- liftIO logFooter
    $(logInfo) $ pack ftr

runELynxLoggingT
  :: (MonadBaseControl IO m, MonadIO m)
  => LogLevel
  -> Force
  -> Maybe FilePath
  -> LoggingT m a
  -> m a
runELynxLoggingT lvl _ Nothing =
  runELynxStderrLoggingT . filterLogger (\_ l -> l >= lvl)
runELynxLoggingT lvl frc (Just fn) =
  runELynxFileLoggingT frc fn . filterLogger (\_ l -> l >= lvl)

runELynxFileLoggingT
  :: MonadBaseControl IO m => Force -> FilePath -> LoggingT m a -> m a
runELynxFileLoggingT frc fp logger = do
  h <- liftBase $ openFile' frc fp WriteMode
  liftBase (hSetBuffering h LineBuffering)
  r <- runLoggingT logger (output2H stderr h)
  liftBase (hClose h)
  return r

runELynxStderrLoggingT :: MonadIO m => LoggingT m a -> m a
runELynxStderrLoggingT = (`runLoggingT` output stderr)

output :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
output h _ _ _ msg = B.hPutStrLn h ls where ls = fromLogStr msg

output2H :: Handle -> Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
output2H h1 h2 _ _ _ msg = do
  B.hPutStrLn h1 ls
  B.hPutStrLn h2 ls
  where ls = fromLogStr msg
