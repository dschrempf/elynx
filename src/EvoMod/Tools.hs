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
  (
    -- * ByteString handling.
    alignRight
  , alignLeft
  , summarizeByteString
  , c2w
  , w2c
    -- * Equality tests.
  , allEqual
  , nearlyEqWith
  , nearlyEq
  , nearlyEqVecWith
  , nearlyEqVec
  , nearlyEqMatWith
  , nearlyEqMat
    -- * Numerics.
  , harmonic
    -- * Input, output.
  , readGZFile
  , writeGZFile
    -- * Parsing.
  , runParserOnFile
  , parseFileWith
  , parseByteStringWith
    -- * Weird stuff :).
  , compose
  , allValues
    -- * Vectors.
  , normalizeSumVec
  , uniformVec
    -- * Matrices.
  , matrixSeparateSymSkew
  , matrixSetDiagToZero
  ) where

import           Codec.Compression.GZip     (compress, decompress)
import           Data.ByteString.Internal   (c2w, w2c)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List                  (isSuffixOf)
import           Numeric.LinearAlgebra      hiding ((<>))
import           Text.Megaparsec

import           EvoMod.Definitions

-- | For a given width, align string to the right; trim on the left if string is
-- longer.
alignRight :: Int -> B.ByteString -> B.ByteString
alignRight n s = B.replicate (fromIntegral n - l) ' ' <> B.take (fromIntegral n) s
  where l = B.length s

-- | For a given width, align string to the left; trim on the right if string is
-- longer.
alignLeft :: Int -> B.ByteString -> B.ByteString
alignLeft n s = B.replicate (fromIntegral n - l) ' ' <> B.take (fromIntegral n) s
  where l = B.length s

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

-- | Parse a 'B.ByteString' and extract the result.
parseByteStringWith :: (ShowErrorComponent e) => Parsec e B.ByteString a -> B.ByteString -> a
parseByteStringWith p f = case parse p "" f of
                            Left  err -> error $ errorBundlePretty err
                            Right val -> val

-- | If a string is longer than a given value, trim it and add some dots.
summarizeByteString :: Int -> B.ByteString -> B.ByteString
summarizeByteString l s | B.length s >= fromIntegral l = B.take (fromIntegral l) s <> B.pack "..."
                        | otherwise = s

-- | Chain a list of functions together. See https://wiki.haskell.org/Compose.
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

-- | Get all values of a bounded enumerated type.
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

-- | Calculate the nth harmonic number.
harmonic :: Int -> Double
harmonic 1 = 1.0
harmonic n = 1.0 / fromIntegral n + harmonic (n-1)

-- | Separate a square matrix into a symmetric and a skew-symmetric matrix.
matrixSeparateSymSkew :: Matrix R -> (Matrix R, Matrix R)
matrixSeparateSymSkew m = (mSym, mSkew)
  where trM = tr m
        mSym  = scale 0.5 $ m + trM
        mSkew = scale 0.5 $ m - trM

-- | Set the diagonal entries of a matrix to zero.
matrixSetDiagToZero :: Matrix R -> Matrix R
matrixSetDiagToZero m = m - diag (takeDiag m)

-- | Test for equality with given tolerance (needed because of machine precision).
nearlyEqWith :: Double -> Double -> Double -> Bool
nearlyEqWith tol a b = tol > abs (a-b)

-- | Test for equality with predefined tolerance (needed because of machine precision).
nearlyEq :: Double -> Double -> Bool
nearlyEq = nearlyEqWith eps

-- | Test if the given number is nearly equal to all elements of a vector.
nearlyEqValListWith :: Double -> Double -> [Double] -> Bool
nearlyEqValListWith tol a = all (nearlyEqWith tol a)

-- | Test if two vectors are nearly equal.
nearlyEqVecWith :: Double -> Vector R -> Vector R -> Bool
nearlyEqVecWith tol a b = nearlyEqValListWith tol 0 (toList $ a - b)

-- | Test if two vectors are nearly equal.
nearlyEqVec :: Vector R -> Vector R -> Bool
nearlyEqVec = nearlyEqVecWith eps

-- | Test if two vectors are nearly equal.
nearlyEqMatWith :: Double -> Matrix R -> Matrix R -> Bool
nearlyEqMatWith tol a b = nearlyEqValListWith tol 0 (concat . toLists $ a - b)

-- | Test if two vectors are nearly equal.
nearlyEqMat :: Matrix R -> Matrix R -> Bool
nearlyEqMat = nearlyEqMatWith eps

-- | Normalize a vector such that elements sum to a given value. See 'normalize' but with 1-norm.
normalizeSumVec :: Double -> Vector R -> Vector R
normalizeSumVec c v = v * scalar c'
  where s = sumElements v
        c' = c/s

-- | A uniform vector of given length.
uniformVec :: Int -> Vector R
uniformVec n = vector $ replicate n (1 / fromIntegral n)
