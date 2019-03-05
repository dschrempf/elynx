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
  quiet  :: l -> Bool
  mHandle :: l -> Maybe Handle

  logS :: String -> ReaderT l IO ()
  logS msg = do
    q <- quiet <$> ask
    unless q $ logSForce msg

  logSForce :: String -> ReaderT l IO ()
  logSForce msg = do
    mh <- mHandle <$> ask
    lift $ putStrLn msg
    case mh of
      Nothing -> return ()
      Just h  -> lift $ hPutStrLn h msg

  logLBS :: LC.ByteString -> ReaderT l IO ()
  logLBS = logS . LC.unpack

  logLBSForce :: LC.ByteString -> ReaderT l IO ()
  logLBSForce = logSForce . LC.unpack
