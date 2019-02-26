{- |
Module      :  EvoMod.Tools.ByteString
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:24:53 2019.

indispensable tools for ByteString handling :).

-}

module EvoMod.Tools.ByteString
  (
    -- * ByteString handling.
    alignRightWith
  , alignRight
  , alignLeftWith
  , alignLeft
  , summarizeByteString
  , c2w
  , w2c
  ) where

import           Data.ByteString.Internal   (c2w, w2c)
import qualified Data.ByteString.Lazy.Char8 as B

-- | For a given width, align string to the right; use given fill character;
-- trim on the left if string is longer.
alignRightWith :: Char -> Int -> B.ByteString -> B.ByteString
alignRightWith c n s = B.replicate (fromIntegral n - l) c <> B.take (fromIntegral n) s
  where l = B.length s

-- | For a given width, align string to the right; trim on the left if string is
-- longer.
alignRight :: Int -> B.ByteString -> B.ByteString
alignRight = alignRightWith ' '

-- | For a given width, align string to the left; use given fill character; trim
-- on the right if string is longer.
alignLeftWith :: Char -> Int -> B.ByteString -> B.ByteString
alignLeftWith c n s = B.replicate (fromIntegral n - l) c <> B.take (fromIntegral n) s
  where l = B.length s

-- | For a given width, align string to the left; trim on the right if string is
-- longer.
alignLeft :: Int -> B.ByteString -> B.ByteString
alignLeft = alignLeftWith ' '


-- | If a string is longer than a given value, trim it and add some dots.
summarizeByteString :: Int -> B.ByteString -> B.ByteString
summarizeByteString l s | B.length s >= fromIntegral l = B.take (fromIntegral l) s <> B.pack "..."
                        | otherwise = s

