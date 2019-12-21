{- |
Module      :  ELynx.Tools.ByteString
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:24:53 2019.

indispensable tools for ByteString handling :).

-}

module ELynx.Tools.ByteString
  (
    -- * ByteString handling.
    alignRightWith
  , alignRight
  , alignLeftWith
  , alignLeft
  , summarizeByteString
  , c2w
  , w2c
  , bshow
  ) where

import           Data.ByteString.Internal   (c2w, w2c)
import qualified Data.ByteString.Lazy.Char8 as L

-- | For a given width, align string to the right; use given fill character;
-- trim on the left if string is longer.
alignRightWith :: Char -> Int -> L.ByteString -> L.ByteString
alignRightWith c n s = L.replicate (fromIntegral n - l) c <> L.take (fromIntegral n) s
  where l = L.length s

-- | For a given width, align string to the right; trim on the left if string is
-- longer.
alignRight :: Int -> L.ByteString -> L.ByteString
alignRight = alignRightWith ' '

-- | For a given width, align string to the left; use given fill character; trim
-- on the right if string is longer.
alignLeftWith :: Char -> Int -> L.ByteString -> L.ByteString
alignLeftWith c n s = L.take (fromIntegral n) s <> L.replicate (fromIntegral n - l) c
  where l = L.length s

-- | For a given width, align string to the left; trim on the right if string is
-- longer.
alignLeft :: Int -> L.ByteString -> L.ByteString
alignLeft = alignLeftWith ' '


-- | If a string is longer than a given value, trim it and add some dots.
summarizeByteString :: Int -> L.ByteString -> L.ByteString
summarizeByteString l s | L.length s >= fromIntegral l = L.take (fromIntegral l) s <> L.pack "..."
                        | otherwise = s

-- | Conversion to 'L.ByteString'.
bshow :: Show a => a -> L.ByteString
bshow = L.pack . show
