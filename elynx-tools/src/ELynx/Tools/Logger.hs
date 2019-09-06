{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Tools.Logger
Description :  Log messages
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Mar  5 22:08:50 2019.

Monad that supports some basic logging features.

TODO: Use monad-logger, https://hackage.haskell.org/package/monad-logger-0.3.30.

-}

module ELynx.Tools.Logger
  (
    -- * Logging
    Logger (..)
  , logQuiet
  , logWarning
  , logInfo
  , logDebug
  , setupLogger
  -- * Helper functions
  , logNewSection
  , reportCapability
  ) where

import           Control.Concurrent         (myThreadId, threadCapability)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Time
import           System.IO

import           ELynx.Tools.ByteString
import           ELynx.Tools.Options        (Verbosity (..))

-- | A logger knows if it has to be quiet and where it deposits more and less
-- useful messages. It also comes with some convenience logging functions in the
-- ReaderT monad transformer. They can be used like this
--
-- @
--   type MyLoggerReader = ReaderT l IO
--
--   myFunc :: (Logger l) => ReaderT l IO ()
--   myFunc = logS "Noooooo."
-- @

class Logger l where
  getVerbosity :: l -> Verbosity
  getHandle   :: l -> Handle

instance Logger (IO a) where
  getVerbosity _ = Info
  getHandle _   = stderr

toLogStr :: Verbosity -> UTCTime -> L.ByteString -> L.ByteString
toLogStr l t s = "[" <> L.pack t' <> "]" <> alignLeft 8 ("[" <> L.pack l' <> "]") <> s
  where
    l' = show l
    t' = formatTime defaultTimeLocale "%B %-e, %Y, at %H:%M %P, %Z." t

-- | For a given 'Verbosity' level, log 'LC.ByteString'.
logWith :: Logger l => Verbosity -> L.ByteString -> ReaderT l IO ()
logWith l s = do
  v <- getVerbosity <$> ask
  h <- getHandle <$> ask
  t <- lift getCurrentTime
  let s' = toLogStr l t s
  when (l <= v) (lift $ L.hPutStrLn h s')

-- | Always print 'LC.ByteString' to screen; even when verbosity level is 'Quiet'.
logQuiet :: Logger l => L.ByteString -> ReaderT l IO ()
logQuiet = logWith Quiet

-- | Print a warning, if verbosity is 'Warning' or higher.
logWarning :: Logger l => L.ByteString -> ReaderT l IO ()
logWarning = logWith Warning

-- | Print informational log message if verbosity is 'Info' or higher.
logInfo :: Logger l => L.ByteString -> ReaderT l IO ()
logInfo = logWith Info

-- | Only print 'LC.ByteString' to screen if verbosity level is 'Debug'.
logDebug :: Logger l => L.ByteString -> ReaderT l IO ()
logDebug = logWith Debug

-- | Setup log handle, if not quiet and if filename is given.
setupLogger :: Maybe FilePath    -- ^ Log file base name.
            -> IO Handle
setupLogger Nothing   = return stderr
setupLogger (Just fn) = openFile (fn ++ ".log") AppendMode

-- | Convenience function. Create a visibly noticeable log entry.
logNewSection :: Logger l => L.ByteString -> ReaderT l IO ()
logNewSection h = do
  logInfo ""
  logInfo $ "-- " <> h

-- | Report the core this thread is running on.
reportCapability :: Logger l => ReaderT l IO ()
reportCapability = do
  i <- lift myThreadId
  (c, _) <- lift $ threadCapability i
  logInfo $ "Running on core: " <> L.pack (show c)
