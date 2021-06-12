{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  ELynx.Tools.InputOutput
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Feb 14 13:30:37 2019.
--
-- Tools involving input, output, and parsing.
module ELynx.Tools.InputOutput
  ( -- * Input, output
    getOutFilePath,
    openFile',
    readGZFile,
    writeGZFile,
    out,
    outHandle,

    -- * Parsing
    runParserOnFile,
    parseFileWith,
    parseIOWith,
    parseFileOrIOWith,
    parseStringWith,
    parseByteStringWith,
  )
where

import Codec.Compression.GZip
  ( compress,
    decompress,
  )
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ask)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isSuffixOf)
import qualified Data.Text as T
import ELynx.Tools.Reproduction
  ( Arguments (..),
    ELynx,
    Force (..),
    Reproducible (..),
    forceReanalysis,
    outFileBaseName,
  )
import System.Directory (doesFileExist)
import System.IO

-- | Get out file path with extension.
getOutFilePath ::
  forall a. Reproducible a => String -> ELynx a (Maybe FilePath)
getOutFilePath ext = do
  a <- ask
  let bn = outFileBaseName . global $ a
      sfxs = outSuffixes a
  if ext `elem` sfxs
    then return $ (++ ext) <$> bn
    else
      error
        "getOutFilePath: out file suffix not registered; please contact maintainer."

checkFile :: Force -> FilePath -> IO ()
checkFile (Force True) _ = return ()
checkFile (Force False) fp =
  doesFileExist fp >>= \case
    True ->
      error $
        "File exists: "
          <> fp
          <> ". Please use the --force option to repeat an analysis."
    False -> return ()

-- | Open existing files only if 'Force' is true.
openFile' :: Force -> FilePath -> IOMode -> IO Handle
openFile' frc fp md = checkFile frc fp >> openFile fp md

-- XXX: For now, all files are read strictly (see help of
-- Control.DeepSeq.force).
readFile' :: FilePath -> IO BL.ByteString
readFile' fn = withFile fn ReadMode $ (evaluate . force) <=< BL.hGetContents

-- | Read file. If file path ends with ".gz", assume gzipped file and decompress
-- before read.
readGZFile :: FilePath -> IO BL.ByteString
readGZFile f
  | ".gz" `isSuffixOf` f = decompress <$> readFile' f
  | otherwise = readFile' f

-- | Write file. If file path ends with ".gz", assume gzipped file and compress
-- before write.
writeGZFile :: Force -> FilePath -> BL.ByteString -> IO ()
writeGZFile frc f r
  | ".gz" `isSuffixOf` f = checkFile frc f >> BL.writeFile f (compress r)
  | otherwise = checkFile frc f >> BL.writeFile f r

-- | Parse a possibly gzipped file.
runParserOnFile :: Parser a -> FilePath -> IO (Either String a)
runParserOnFile p f = eitherResult . parse p <$> readGZFile f

-- | Parse a possibly gzipped file and extract the result.
parseFileWith :: Parser a -> FilePath -> IO a
parseFileWith p f = parseFileOrIOWith p (Just f)

-- | Parse standard input.
parseIOWith :: Parser a -> IO a
parseIOWith p = parseFileOrIOWith p Nothing

-- | Parse a possibly gzipped file, or standard input, and extract the result.
parseFileOrIOWith :: Parser a -> Maybe FilePath -> IO a
parseFileOrIOWith p mf = do
  s <- maybe BL.getContents readGZFile mf
  return $ parseByteStringWith p s

-- | Parse a 'String' and extract the result.
parseStringWith :: Parser a -> String -> a
parseStringWith p x = parseByteStringWith p (BL.pack x)

-- | Parse a 'BL.ByteString' and extract the result.
parseByteStringWith :: Parser a -> BL.ByteString -> a
parseByteStringWith p x = case eitherResult $ parse p x of
  Left err -> error err
  Right val -> val

-- | Write a result with a given name to file with given extension or standard
-- output. Supports compression.
out :: Reproducible a => String -> BL.ByteString -> String -> ELynx a ()
out name res ext = do
  mfp <- getOutFilePath ext
  case mfp of
    Nothing -> do
      $(logInfo) $ T.pack $ "Write " <> name <> " to standard output."
      liftIO $ BL.putStr res
    Just fp -> do
      $(logInfo) $ T.pack $ "Write " <> name <> " to file '" <> fp <> "'."
      frc <- forceReanalysis . global <$> ask
      liftIO $ writeGZFile frc fp res

-- | Get an output handle, does not support compression. The handle has to be
-- closed after use!
outHandle :: Reproducible a => String -> String -> ELynx a Handle
outHandle name ext = do
  mfp <- getOutFilePath ext
  case mfp of
    Nothing -> do
      $(logInfo) $ T.pack $ "Write " <> name <> " to standard output."
      return stdout
    Just fp -> do
      $(logInfo) $ T.pack $ "Write " <> name <> " to file '" <> fp <> "'."
      frc <- forceReanalysis . global <$> ask
      liftIO $ openFile' frc fp WriteMode
