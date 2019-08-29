{- |
Module      :  EvoMod.Tools.Logger
Description :  Log messages
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Mar  5 22:08:50 2019.

Monad that supports some basic logging features.

-}

module EvoMod.Tools.Logger
  (
    -- * Logging
    Logger (..)
  , logSWith
  , logS
  , logSDebug
  , logLBSWith
  , logLBS
  , logLBSDebug
  , setupLogger
  , closeLogger
  -- * Warnings
  , warnS
  , warnLBS
  -- * Helper functions
  , logNewSection
  , reportCapability
  ) where

import           Control.Concurrent         (myThreadId, threadCapability)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as LC
import           System.IO

import           EvoMod.Tools.Options       (Verbosity (..))

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
  verbosity :: l -> Verbosity
  mHandle   :: l -> Maybe Handle

logHandle :: Maybe Handle -> String -> IO ()
logHandle Nothing  _   = return ()
logHandle (Just h) msg = hPutStrLn h msg

-- | For a given 'Verbosity' level, log 'String'.
logSWith :: Logger l => Verbosity -> String -> ReaderT l IO ()
logSWith lvl msg = do
  v  <- verbosity <$> ask
  when (lvl <= v) (lift $ hPutStrLn stderr msg)
  mh <- mHandle <$> ask
  lift $ logHandle mh msg

-- | Always print 'String' to screen; even when verbosity level is 'Quiet'.
warnS :: Logger l => String -> ReaderT l IO ()
warnS = logSWith Quiet

-- | If not quiet, print 'String' to screen.
logS :: Logger l => String -> ReaderT l IO ()
logS = logSWith Info

-- | Only print 'String' to screen if verbosity level is 'Debug'.
logSDebug :: Logger l => String -> ReaderT l IO ()
logSDebug = logSWith Debug

-- | See 'logSWith'; but for lazy byte strings.
logLBSWith :: Logger l => Verbosity -> LC.ByteString -> ReaderT l IO ()
logLBSWith lvl = logSWith lvl . LC.unpack

-- | See 'warnS'; but for lazy byte strings.
warnLBS :: Logger l => LC.ByteString -> ReaderT l IO ()
warnLBS = warnS . LC.unpack

-- | See 'logS'; but for lazy byte strings.
logLBS :: Logger l => LC.ByteString -> ReaderT l IO ()
logLBS = logS . LC.unpack

-- | See 'logSDebug'; but for lazy byte strings.
logLBSDebug :: Logger l => LC.ByteString -> ReaderT l IO ()
logLBSDebug = logSDebug . LC.unpack

-- | Setup log handle, if not quiet and if filename is given.
setupLogger :: Maybe FilePath    -- ^ Log file base name.
            -> IO (Maybe Handle)
setupLogger Nothing   = return Nothing
setupLogger (Just fn) = Just <$> openFile (fn ++ ".log") AppendMode

-- | Close the logging file handle.
closeLogger :: Maybe Handle -> IO ()
-- It took me quite a while to find this out.
closeLogger = mapM_ hClose

-- | Convenience function. Create a visibly noticeable log entry.
logNewSection :: Logger l => String -> ReaderT l IO ()
logNewSection h = do
  logS ""
  logS $ "-- " ++ h

-- | Report the core this thread is running on.
reportCapability :: Logger l => ReaderT l IO ()
reportCapability = do
  i <- lift myThreadId
  (c, _) <- lift $ threadCapability i
  logS $ "Running on core: " ++ show c
