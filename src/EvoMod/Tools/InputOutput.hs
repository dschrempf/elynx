{- |
Module      :  EvoMod.Tools.InputOutput
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:30:37 2019.

Tools involving input, output, and parsing.

-}

module EvoMod.Tools.InputOutput
  (
    -- * Input, output.
    readGZFile
  , writeGZFile
    -- * Parsing.
  , runParserOnFile
  , parseFileWith
  , parseByteStringWith
  ) where

import           Codec.Compression.GZip     (compress, decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List                  (isSuffixOf)
import           Text.Megaparsec

-- | Read file. If file path ends with ".gz", assume gzipped file and decompress
-- before read.
readGZFile :: FilePath -> IO B.ByteString
readGZFile f | ".gz" `isSuffixOf` f = decompress <$> B.readFile f
             | otherwise            = B.readFile f

-- | Write file. If file path ends with ".gz", assume gzipped file and compress
-- before write.
writeGZFile :: FilePath -> B.ByteString -> IO ()
writeGZFile f | ".gz" `isSuffixOf` f = B.writeFile f . compress
              | otherwise            = B.writeFile f

-- | Parse a possibly gzipped file.
runParserOnFile :: Parsec e B.ByteString a -> FilePath -> IO (Either (ParseErrorBundle B.ByteString e) a)
runParserOnFile p f = parse p f <$> readGZFile f

-- | Parse a possibly gzipped file and extract the result.
parseFileWith :: (ShowErrorComponent e) => Parsec e B.ByteString a -> FilePath -> IO a
parseFileWith p f = do res <- runParserOnFile p f
                       case res of
                         Left  err -> error $ errorBundlePretty err
                         Right val -> return val

-- | Parse a 'B.ByteString' and extract the result.
parseByteStringWith :: (ShowErrorComponent e) => Parsec e B.ByteString a -> B.ByteString -> a
parseByteStringWith p f = case parse p "" f of
                            Left  err -> error $ errorBundlePretty err
                            Right val -> val
