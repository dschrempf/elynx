{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  ELynx.Tools.Reproduction
Description :  Functions to ease reproduction of analyses
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Nov 19 15:07:09 2019.

-}

module ELynx.Tools.Reproduction
  ( ELynx
  , getOutFilePath
  , Reproducible (..)
  , Reproduction (..)
  , readR
  , writeR
  ) where

import           Control.Monad              (zipWithM)
import           Control.Monad.Logger       (LoggingT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Crypto.Hash.SHA256         (hash)
import           Data.Aeson                 (FromJSON, ToJSON,
                                             eitherDecodeFileStrict',
                                             encodeFile)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Char8      as B
import           Data.Either                (either)
import           GHC.Generics               (Generic)
import           Options.Applicative        (Parser, briefDesc, defaultPrefs,
                                             execParserPure, getParseResult,
                                             info)
import           System.Environment         (getArgs, getProgName)

import           ELynx.Tools.Options

-- TODO: Do these two definitions belong here?

-- | Logging transformer to be used with all executables.
type ELynx = LoggingT (ReaderT GlobalArguments IO)

getOutFilePath :: String -> ELynx (Maybe FilePath)
getOutFilePath ext = do
  ofbn <- outFileBaseName <$> lift ask
  return $ (++ ext) <$> ofbn

-- | Reproducible commands have a set of input files that have to be checked for
-- consistency.
class Reproducible a where
  inFiles :: a -> [FilePath]
  parser  :: a -> Parser a

-- | Necessary information for a reproducible run. Notably, the input files are
-- checked for consistency!
data Reproduction a = Reproduction
  { progName  :: String         -- ^ Program name.
  , args      :: [String]       -- ^ Command line arguments without program name.
  , filePaths :: [FilePath]     -- ^ File paths of in files.
  , checkSums :: [String]       -- ^ SHA256 sums of in files.
  , cmd       :: a              -- ^ Command argument.
  } deriving (Generic)

instance ToJSON a => ToJSON (Reproduction a) where

instance FromJSON a => FromJSON (Reproduction a)

-- Does the command line fit the provided command?
checkArgs :: (Eq a, Show a, Reproducible a)
          => [String] -> a -> IO (Either String ())
checkArgs as c = do
  let p    = parser c
      pres = execParserPure defaultPrefs (info p briefDesc) as
  return $ case getParseResult pres of
    Nothing  ->
      Left $ unlines [ "Could not parse command line string:"
                     , concat as ]
    Just c'  ->
      if c' /= c
      then Left $ unlines [ "Command line string and command arguments do not fit:"
                          , concat as
                          , show c ]
      else Right ()

-- Does the file match the checksum?
checkFile :: FilePath -> B.ByteString -> IO (Either String ())
checkFile fp h = do
  h' <- hashFile fp
  return $ if h' == h
    then Right ()
    else Left $ unlines [ "SHA256 sum does not match for a file."
                        , fp ++ " has check sum " ++ B.unpack h'
                        , "Stored sum is " ++ B.unpack h ]

-- | Check if command line arguments and files check sums are matching.
checkReproduction :: (Eq a, Show a, Reproducible a)
                  => Reproduction a -> IO (Either String ())
checkReproduction (Reproduction _ as fs ss c) = do
  chA  <- checkArgs as c
  chFs <- zipWithM checkFile fs (map B.pack ss)
  let ch = sequence_ (chA : chFs)
  return $ first ("Failed validating the reproduction file.\n" ++) ch

-- | Read an ELynx reproduction file. Check consistency of arguments and input files.
readR :: forall a . (Eq a, Show a, Reproducible a, FromJSON a)
      => FilePath -> IO (Reproduction a)
readR fp = do
  res <- eitherDecodeFileStrict' fp :: IO (Either String (Reproduction a))
  case res of
    Left err -> do
      putStrLn "Failed reading the ELynx reproduction file."
      putStrLn "The following error was encountered."
      error err
    Right r  -> do
      ch <- checkReproduction r
      return $ either error (const r) ch

-- | Helper function.
hashFile :: FilePath -> IO B.ByteString
hashFile f = hash <$> B.readFile f

-- | Write an ELynx reproduction file. Check arguments.
writeR :: (Eq a, Show a, Reproducible a, ToJSON a) => FilePath -> a -> IO ()
writeR fp c = do
  p  <- getProgName
  as <- getArgs
  let fs = inFiles c
  cs <- mapM hashFile fs
  let
    cs' = map B.unpack cs
    r   = Reproduction p as fs cs' c
  -- XXX: Actually, it is only necessary to to checkArgs here. But let's just be safe.
  ch <- checkReproduction r
  either error (const $ encodeFile fp r) ch
