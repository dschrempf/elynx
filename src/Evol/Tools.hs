{- |
Module      :  Evol.Tools
Description :  Auxiliary tools.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 13:27:56 2018.

-}


module Evol.Tools
  ( alignRight
  , alignLeft
  , allEqual
  , c2w
  , w2c
  , readGZFile
  , writeGZFile
  ) where

import           Codec.Compression.GZip   (compress, decompress)
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy     as B (ByteString, readFile, writeFile)
import           Data.List                (isSuffixOf)

alignRight :: Int -> String -> String
alignRight n s | l > n     = take n s
               | otherwise = replicate (n-l) ' ' ++ s
               where l = length s

alignLeft :: Int -> String -> String
alignLeft n s | l > n     = take n s
              | otherwise = s ++ replicate (n-l) ' '
               where l = length s

allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

readGZFile :: FilePath -> IO B.ByteString
readGZFile f | ".gz" `isSuffixOf` f = decompress <$> B.readFile f
             | otherwise            = B.readFile f

writeGZFile :: FilePath -> B.ByteString -> IO ()
writeGZFile f | ".gz" `isSuffixOf` f = B.writeFile f . compress
              | otherwise            = B.writeFile f
