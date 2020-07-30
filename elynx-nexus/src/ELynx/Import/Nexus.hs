{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Import.Nexus
-- Description :  Nexus types and classes
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 17:10:05 2020.
module ELynx.Import.Nexus
  ( Parser,
    Block (..),
    nexus,
  )
where

import Control.Monad (void)
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Byte

-- | Megaparsec shortcut.
type Parser = Parsec Void ByteString

-- | A Nexus block has a name (e.g., TREES), and parser for the entry.
data Block a = Block
  { name :: ByteString,
    parser :: Parser a
  }

-- This has to be refined. Like this, only one block can be parsed, and the
-- block type has to be known beforehand.

-- | Parse a Nexus file with a given 'Block'.
nexus :: Block a -> Parser a
nexus b = start *> block b

start :: Parser ()
start = void $ string "#NEXUS" <* space

block :: Block a -> Parser a
block b = between (begin $ name b) end (parser b)

begin :: ByteString -> Parser ()
begin n = do
  _ <- string "BEGIN"
  _ <- space
  _ <- string n
  _ <- char (c2w ';')
  _ <-
    space
      <?> "block-begin"
  return ()

end :: Parser ()
end = void $ string "END;" <* space
