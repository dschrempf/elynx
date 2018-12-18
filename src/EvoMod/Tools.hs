{- |
Module      :  EvoMod.Tools
Description :  Auxiliary tools.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Indispensable tools.

-}


module EvoMod.Tools
  ( alignRight
  , alignLeft
  , allEqual
  , c2w
  , w2c
  , readGZFile
  , writeGZFile
  , runParserOnFile
  , parseFileWith
  , parseByteStringWith
  , showWithoutQuotes
  , summarizeString
  , compose
  , allValues
  ) where

import           Codec.Compression.GZip   (compress, decompress)
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy     as B (ByteString, readFile, writeFile)
import           Data.List                (isSuffixOf)
import           Text.Megaparsec

import EvoMod.Defaults (defStringSummaryLength)

-- | For a given width, align string to the right.
alignRight :: Int -> String -> String
alignRight n s | l > n     = take n s
               | otherwise = replicate (n-l) ' ' ++ s
               where l = length s

-- | For a given width, align string to the left.
alignLeft :: Int -> String -> String
alignLeft n s | l > n     = take n s
              | otherwise = s ++ replicate (n-l) ' '
               where l = length s

-- | Test if all elements of a list are equal.
allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

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

-- | Parse a 'ByteString' and extract the result.
parseByteStringWith :: (ShowErrorComponent e) => Parsec e B.ByteString a -> B.ByteString -> a
parseByteStringWith p f = case parse p "" f of
                            Left  err -> error $ errorBundlePretty err
                            Right val -> val

rmFirstQuote :: String -> String
rmFirstQuote ('\"':xs) = xs
rmFirstQuote xs        = xs

rmLastQuote :: String -> String
rmLastQuote = reverse . rmFirstQuote . reverse

rmDoubleQuotes :: String -> String
rmDoubleQuotes = rmFirstQuote . rmLastQuote

-- | Show a string without quotes ... (sometimes Haskell annoys me :D).
showWithoutQuotes :: Show a => a -> String
showWithoutQuotes = rmDoubleQuotes . show

-- | If a string is longer than 'defStringSummaryLength', trim it and add some dots.
summarizeString :: String -> String
summarizeString s | length s >= defStringSummaryLength = take defStringSummaryLength s ++ "..."
                  | otherwise = s

-- | See https://wiki.haskell.org/Compose.
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

-- | Get all values of a bounded enumerated type.
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]
