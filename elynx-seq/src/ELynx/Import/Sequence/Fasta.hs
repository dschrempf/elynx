{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  ELynx.Import.Sequence.Fasta
-- Description :  Import Fasta sequences
-- Copyright   :  (c) Dominik Schrempf 2018
-- License     :  GPL-3.0-or-later
--
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Parse FASTA files.
--
-- [NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).
--
-- For more complicated parsers, try to use a [lexer](https://hackage.haskell.org/package/megaparsec-7.0.1/docs/Text-Megaparsec-Byte-Lexer.html).
module ELynx.Import.Sequence.Fasta
  ( Parser,
    fastaSequence,
    fasta,
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Set as S
import Data.Word8
  ( Word8,
    isAlphaNum,
  )
import ELynx.Data.Alphabet.Alphabet as A
import ELynx.Data.Alphabet.Character
import ELynx.Data.Sequence.Sequence
import ELynx.Tools
import Prelude hiding (takeWhile)

isSpecial :: Word8 -> Bool
isSpecial w = w `elem` map c2w ['_', '|', '.', '-']

isHeader :: Word8 -> Bool
isHeader w = isAlphaNum w || isSpecial w

sequenceHeader :: Parser (L.ByteString, L.ByteString)
sequenceHeader = do
  _ <- C.char '>'
  n <- takeWhile1 isHeader
  _ <- takeWhile C.isHorizontalSpace
  d <- takeWhile isHeader
  _ <- C.endOfLine
  return (L.fromStrict n, L.fromStrict d)

-- It is a little faster to directly pass the set of allowed characters. Then,
-- this set only has to be calculcated once per sequence in 'fastaSequence'.
sequenceLine :: S.Set Word8 -> Parser L.ByteString
sequenceLine s = do
  -- XXX: Will fail for non-capital letters.
  !xs <- takeWhile1 (`S.member` s)
  return (L.fromStrict xs)

-- XXX: If sequences are parsed line by line, the lines have to be copied when
-- forming the complete sequence. This is not memory efficient.

-- | Parse a sequence of characters.
fastaSequence :: Alphabet -> Parser Sequence
fastaSequence a = do
  (n, d) <- sequenceHeader
  let !alph = S.map toWord (A.all . alphabetSpec $ a)
  lns <- sequenceLine alph `sepBy1` C.endOfLine
  _ <- many C.endOfLine
  return $ Sequence n d a (fromByteString $ L.concat lns)

-- | Parse a Fasta file with given 'Alphabet'.
fasta :: Alphabet -> Parser [Sequence]
fasta a = some (fastaSequence a) <* endOfInput
