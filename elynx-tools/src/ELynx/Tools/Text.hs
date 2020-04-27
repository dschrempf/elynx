{- |
Module      :  ELynx.Tools.Text
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Feb 14 13:24:53 2019.

indispensable tools for ByteString handling :).

-}

module ELynx.Tools.Text
  ( -- * Text handling
    tShow
  , fromBs
  )
where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )

-- | Conversion to 'Text' from showable values.
tShow :: Show a => a -> Text
tShow = pack . show

-- | Conversion to 'Text' from bytestring.
fromBs :: ByteString -> Text
fromBs = toStrict . decodeUtf8

