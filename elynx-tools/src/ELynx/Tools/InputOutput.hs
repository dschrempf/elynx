{- |
Module      :  ELynx.Tools.InputOutput
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:30:37 2019.

Tools involving input, output, and parsing.

-}

module ELynx.Tools.InputOutput
  (
    -- * Input, output.
    readGZFile
  , writeGZFile
  , io
    -- * Parsing.
  , runParserOnFile
  , parseFileWith
  , parseFileOrIOWith
  , parseStringWith
  , parseByteStringWith
  ) where

import           Codec.Compression.GZip     (compress, decompress)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (isSuffixOf)
import           Data.Maybe
import           Text.Megaparsec

import           ELynx.Tools.Logger

-- | Read file. If file path ends with ".gz", assume gzipped file and decompress
-- before read.
readGZFile :: FilePath -> IO L.ByteString
readGZFile f | ".gz" `isSuffixOf` f = decompress <$> L.readFile f
             | otherwise            = L.readFile f

-- | Write file. If file path ends with ".gz", assume gzipped file and compress
-- before write.
writeGZFile :: FilePath -> L.ByteString -> IO ()
writeGZFile f | ".gz" `isSuffixOf` f = L.writeFile f . compress
              | otherwise            = L.writeFile f

-- | Parse a possibly gzipped file.
runParserOnFile :: Parsec e L.ByteString a
                -> FilePath -> IO (Either (ParseErrorBundle L.ByteString e) a)
runParserOnFile p f = parse p f <$> readGZFile f

-- | Parse a possibly gzipped file and extract the result.
parseFileWith :: (ShowErrorComponent e)
                  => Parsec e L.ByteString a -- ^ The parser.
                  -> FilePath
                  -> IO a
parseFileWith p f = parseFileOrIOWith p (Just f)

-- | Parse a possibly gzipped file, or standard input, and extract the result.
parseFileOrIOWith :: (ShowErrorComponent e)
                  => Parsec e L.ByteString a -- ^ The parser.
                  -> Maybe FilePath          -- ^ If no file path is given, standard input is used.
                  -> IO a
parseFileOrIOWith p mf = do
  contents <- case mf of
                Nothing -> L.getContents
                Just f  -> readGZFile f
  return $ parseByteStringWith (fromMaybe "Standard input" mf) p contents

-- | Parse a 'String' and extract the result.
parseStringWith :: (ShowErrorComponent e)
                => String                  -- ^ Name of string.
                -> Parsec e L.ByteString a -- ^ Parser.
                -> String                  -- ^ Input.
                -> a
parseStringWith s p l = parseByteStringWith s p (L.pack l)

-- | Parse a 'L.ByteString' and extract the result.
parseByteStringWith :: (ShowErrorComponent e)
                    => String                  -- ^ Name of byte string.
                    -> Parsec e L.ByteString a -- ^ Parser.
                    -> L.ByteString            -- ^ Input.
                    -> a
parseByteStringWith s p l = case parse p s l of
                            Left  err -> error $ errorBundlePretty err
                            Right val -> val

-- | Write result to file or standard output.
io :: Logger l => L.ByteString -> Maybe FilePath -> ReaderT l IO ()
io res mfp =
  case mfp of
    Nothing -> do
      logS "Write results to standard output."
      lift $ L.putStr res
    Just fp -> do
      logS $ "Write results to file '" ++ fp ++ "'."
      lift $ writeGZFile fp res
