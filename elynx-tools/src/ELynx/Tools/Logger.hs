{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  ELynx.Tools.Logger
-- Description :  Monad logger utility functions
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Sep  6 14:43:19 2019.
module ELynx.Tools.Logger
  ( Verbosity (..),
    HasLock (..),
    HasLogHandles (..),
    HasStartingTime (..),
    HasVerbosity (..),
    Logger,
    logOutB,
    logDebugB,
    logDebugS,
    logWarnB,
    logWarnS,
    logInfoB,
    logInfoS,
    logHeader,
    logInfoHeader,
    logInfoFooter,
    logInfoNewSection,
  )
where

-- TODO: Sort functions.

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Time
import Data.Version
import GHC.Generics
import Language.Haskell.TH
import Paths_elynx_tools
import System.Environment
import System.IO

-- | Verbosity levels.
data Verbosity = Quiet | Warn | Info | Debug
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- TODO: Do I need '' or '?
$(deriveJSON defaultOptions ''Verbosity)

-- | Types with an output lock for concurrent output.
class HasLock e where
  getLock :: e -> MVar ()

-- | Types with logging information.
class HasLogHandles e where
  getLogHandles :: e -> [Handle]

-- | Types with starting time.
class HasStartingTime s where
  getStartingTime :: s -> UTCTime

-- | Types with verbosity.
class HasVerbosity s where
  getVerbosity :: s -> Verbosity

-- | Reader transformer used for logging to a file and to standard output.
type Logger e a = ReaderT e IO a

msgPrepare :: BL.ByteString -> BL.ByteString -> BL.ByteString
msgPrepare pref msg = BL.intercalate "\n" $ map (BL.append pref) $ BL.lines msg

-- Make sure that concurrent output is not scrambled.
atomicAction :: HasLock e => IO () -> Logger e ()
atomicAction a = do
  l <- reader getLock
  liftIO $ withMVar l (const a)

-- | Write to standard output and maybe to log file.
logOutB ::
  (HasLogHandles e, HasLock e) =>
  -- | Prefix.
  BL.ByteString ->
  -- | Message.
  BL.ByteString ->
  Logger e ()
logOutB pref msg = do
  hs <- reader getLogHandles
  mapM_ (atomicAction . (`BL.hPutStrLn` msg')) hs
  where
    msg' = msgPrepare pref msg

-- Perform debug action.
logDebugA :: (HasLock e, HasLogHandles e, HasVerbosity e) => Logger e () -> Logger e ()
logDebugA a = reader getVerbosity >>= \v -> when (v >= Debug) a

-- | Log debug message.
logDebugB :: (HasLock e, HasLogHandles e, HasVerbosity e) => BL.ByteString -> Logger e ()
logDebugB = logDebugA . logOutB "D: "

-- | Log debug message.
logDebugS :: (HasLock e, HasLogHandles e, HasVerbosity e) => String -> Logger e ()
logDebugS = logDebugB . BL.pack

-- Perform warning action.
logWarnA :: (HasLogHandles e, HasVerbosity e) => Logger e () -> Logger e ()
logWarnA a = reader getVerbosity >>= \v -> when (v >= Warn) a

-- | Log warning message.
logWarnB :: (HasLock e, HasLogHandles e, HasVerbosity e) => BL.ByteString -> Logger e ()
logWarnB = logWarnA . logOutB "W: "

-- | Log warning message.
logWarnS :: (HasLock e, HasLogHandles e, HasVerbosity e) => String -> Logger e ()
logWarnS = logWarnB . BL.pack

-- Perform info action.
logInfoA :: (HasLogHandles e, HasVerbosity e) => Logger e () -> Logger e ()
logInfoA a = reader getVerbosity >>= \v -> when (v >= Info) a

-- | Log info message.
logInfoB :: (HasLock e, HasLogHandles e, HasVerbosity e) => BL.ByteString -> Logger e ()
logInfoB = logInfoA . logOutB "   "

-- | Log info message.
logInfoS :: (HasLock e, HasLogHandles e, HasVerbosity e) => String -> Logger e ()
logInfoS = logInfoB . BL.pack

-- Be careful; it is necessary to synchronize the version numbers across packages.
versionString :: String
versionString = "ELynx Suite version " ++ showVersion version ++ "."

copyrightString :: String
copyrightString = "Developed by Dominik Schrempf."

compilationString :: String
compilationString =
  "Compiled on "
    ++ $( stringE
            =<< runIO
              ( formatTime defaultTimeLocale "%B %-e, %Y, at %H:%M %P, %Z."
                  `fmap` Data.Time.getCurrentTime
              )
        )

-- A short header to be used in executables. 'unlines' doesn't work here because
-- it adds an additional newline at the end.
logHeader :: [String]
logHeader = [versionString, copyrightString, compilationString]

time :: IO String
time =
  formatTime defaultTimeLocale "%B %-e, %Y, at %H:%M %P, %Z."
    `fmap` Data.Time.getCurrentTime

-- | Log header.
logInfoHeader :: (HasLock e, HasLogHandles e, HasVerbosity e) => String -> [String] -> Logger e ()
logInfoHeader h dsc = do
  logInfoS (replicate 70 '-')
  logInfoS ("ELynx suite; version " ++ showVersion version <> ".")
  logInfoS "Developed by: Dominik Schrempf."
  logInfoS "License: GPL-3.0-or-later."
  t <- liftIO time
  p <- liftIO getProgName
  as <- liftIO getArgs
  let hdr' =
        intercalate "\n" $
          ("=== " <> h) :
          dsc
            ++ logHeader
            ++ ["Start time: " ++ t, "Command line: " ++ p ++ " " ++ unwords as]
  -- TODO: Check this header, remove redundant information.
  logInfoS hdr'
  logInfoS (replicate 70 '-')

-- TODO: Either use starting time or remove constraint and class.

-- | Log footer.
logInfoFooter :: (HasLock e, HasLogHandles e, HasStartingTime e, HasVerbosity e) => Logger e ()
logInfoFooter = do
  -- TODO: Either:
  t <- liftIO time
  let timeStr = "=== End time: " ++ t
  logInfoS timeStr

-- -- TODO: Or:
-- ti <- reader getStartingTime
-- te <- liftIO getCurrentTime
-- let dt = te `diffUTCTime` ti
-- logInfoB $ "Wall clock run time: " <> renderDuration dt <> "."
-- logInfoS $ "End time: " <> renderTime te

-- | Unified way of creating a new section in the log.
logInfoNewSection :: (HasLock e, HasLogHandles e, HasVerbosity e) => String -> Logger e ()
logInfoNewSection s = logInfoS $ "== " <> s
