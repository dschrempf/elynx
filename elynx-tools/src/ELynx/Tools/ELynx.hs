{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  ELynx.Tools.ELynx
-- Description :  The ELynx transformer
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  2 18:55:11 2021.
module ELynx.Tools.ELynx
  ( forceOpt,
    GlobalArguments (..),
    globalArguments,
    Seed (..),
    seedOpt,
    ELynx,
    Arguments (..),
    parseArguments,
  )
where

import Control.Monad.Trans.Reader
import Data.Aeson
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import GHC.Generics
import Options.Applicative hiding (empty)
import Options.Applicative.Help.Pretty

-- TODO: Sort?

-- TODO: Define environment. Similar to mcmc

-- | ELynx transformer to be used with all executables.
type ELynx a = ReaderT (Environment a) IO

-- | The 'ReaderT' and 'LoggingT' wrapper for ELynx. Prints a header and a
-- footer, logs to 'stderr' if no file is provided. Initializes the seed if none
-- is provided. If a log file is provided, log to the file and to 'stderr'.
eLynxWrapper ::
  forall a b.
  (Eq a, Show a, Reproducible a, ToJSON a) =>
  Arguments a ->
  -- TODO: How can I remove this "extraction of the current command"?
  (Arguments a -> Arguments b) ->
  ELynx b () ->
  IO ()
eLynxWrapper args f worker = do
  -- Arguments.
  let gArgs = global args
      lArgs = local args
  let lvl = toLogLevel $ verbosity gArgs
      rd = forceReanalysis gArgs
      outBn = outFileBaseName gArgs
      logFile = (++ ".log") <$> outBn

  -- TODO! TODO!

  -- 1. Initialize environment (open log files; fix seed?).

  -- TODO: Remove.
  -- runELynxLoggingT lvl rd logFile $ do

  initializeEnvironment
  -- Header.
  logInfoHeader (cmdName @a) (cmdDsc @a)
  -- Fix seed.
  lArgs' <- case getSeed lArgs of
    Nothing -> return lArgs
    Just Random -> do
      g <- liftIO createSystemRandom
      s <- liftIO $ fromSeed <$> save g
      $(logInfo) $ pack $ "Seed: random; set to " <> show s <> "."
      return $ setSeed lArgs s
    Just (Fixed s) -> do
      $(logInfo) $ pack $ "Seed: " <> show s <> "."
      return lArgs
  let args' = Arguments gArgs lArgs'

  -- 2. Run worker.
  runReaderT worker $ f args'

  -- 3. Close environment.
  -- Write reproduction file.
  case (writeElynxFile gArgs, outBn) of
    (False, _) ->
      logInfoS "No elynx file option --- skip writing ELynx file for reproducible runs."
    (True, Nothing) ->
      logInfoS "No output file given --- skip writing ELynx file for reproducible runs."
    (True, Just bn) -> do
      logInfoS "Write ELynx reproduction file."
      liftIO $ writeReproduction bn args'
  -- Footer.
  logInfoFooter

initializeEnvironment :: a
initializeEnvironment = undefined

-- runELynxLoggingT ::
--   (MonadBaseControl IO m, MonadIO m) =>
--   LogLevel ->
--   Force ->
--   Maybe FilePath ->
--   LoggingT m a ->
--   m a
-- runELynxLoggingT lvl _ Nothing =
--   runELynxStderrLoggingT . filterLogger (\_ l -> l >= lvl)
-- runELynxLoggingT lvl frc (Just fn) =
--   runELynxFileLoggingT frc fn . filterLogger (\_ l -> l >= lvl)

-- runELynxFileLoggingT ::
--   MonadBaseControl IO m => Force -> FilePath -> LoggingT m a -> m a
-- runELynxFileLoggingT frc fp logger = do
--   h <- liftBase $ openFile' frc fp WriteMode
--   liftBase (hSetBuffering h LineBuffering)
--   r <- runLoggingT logger (output2H stderr h)
--   liftBase (hClose h)
--   return r

-- runELynxStderrLoggingT :: MonadIO m => LoggingT m a -> m a
-- runELynxStderrLoggingT = (`runLoggingT` output stderr)

-- output :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
-- output h _ _ _ msg = BS.hPutStrLn h ls where ls = fromLogStr msg

-- output2H :: Handle -> Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
-- output2H h1 h2 _ _ _ msg = do
--   BS.hPutStrLn h1 ls
--   BS.hPutStrLn h2 ls
--   where
--     ls = fromLogStr msg
