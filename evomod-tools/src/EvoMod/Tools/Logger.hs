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
  ( Logger (..)
  , logS
  , logSForce
  , logLBS
  , logLBSForce
  , setupLogger
  , closeLogger
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as LC
import           System.IO

-- | A logger knows if it has to be quiet and where it deposits more and less
-- useful messages. It also comes with some convenience logging functions in the
-- ReaderT monad transformer. They can be used like this (assume @l@ is instance
-- of class 'Logger')
--
-- @
--   type MyLoggerReader = ReaderT l IO
--
--   myFunc :: MyLoggerReader ()
--   myFunc = logS "Noooooo."
-- @

class Logger l where
  quiet   :: l -> Bool
  mHandle :: l -> Maybe Handle

-- | If not quiet, log string.
logS :: Logger l => String -> ReaderT l IO ()
logS msg = do
  q <- quiet <$> ask
  unless q $ logSForce msg

logHandle :: Maybe Handle -> String -> IO ()
logHandle Nothing  _   = return ()
logHandle (Just h) msg = hPutStrLn h msg

-- | Log string, ignore quiet option.
logSForce :: Logger l => String -> ReaderT l IO ()
logSForce msg = do
  mh <- mHandle <$> ask
  lift $ putStrLn msg
  lift $ logHandle mh msg

-- | See 'logS' but for lazy byte strings.
logLBS :: Logger l => LC.ByteString -> ReaderT l IO ()
logLBS = logS . LC.unpack

-- | See 'logSForce' but for lazy byte strings.
logLBSForce :: Logger l => LC.ByteString -> ReaderT l IO ()
logLBSForce = logSForce . LC.unpack

-- | Setup log handle, if not quiet and if filename is given.
setupLogger :: Bool -> Maybe FilePath -> IO (Maybe Handle)
setupLogger True _          = return Nothing
setupLogger False Nothing   = return Nothing
setupLogger False (Just fn) = Just <$> openFile (fn ++ ".log") WriteMode

-- | Close the logging file handle.
closeLogger :: Maybe Handle -> IO ()
-- It took me quite a while to find this out.
closeLogger = mapM_ hClose
