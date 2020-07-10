{-# LANGUAGE BangPatterns #-}

{- |
Module      :  ELynx.Import.Sequence.Fasta
Description :  Import Fasta sequences
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3.0-or-later


Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Parse FASTA files.

[NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).

For more complicated parsers, try to use a [lexer](https://hackage.haskell.org/package/megaparsec-7.0.1/docs/Text-Megaparsec-Byte-Lexer.html).
-}


module ELynx.Import.Sequence.Fasta
  ( Parser
  , fastaSequence
  , fasta
  )
where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.Set                      as S
import           Data.Void
import           Data.Word8                     ( Word8
                                                , isAlphaNum
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Byte

import           ELynx.Data.Alphabet.Alphabet  as A
import           ELynx.Data.Alphabet.Character
import           ELynx.Data.Sequence.Sequence
import           ELynx.Tools

-- | Shortcut.
type Parser = Parsec Void L.ByteString

isSpecial :: Word8 -> Bool
isSpecial w = w `elem` map c2w ['_', '|', '.', '-']

isHeader :: Word8 -> Bool
isHeader w = isAlphaNum w || isSpecial w

isHorizontalSpace :: Word8 -> Bool
isHorizontalSpace w = (w == c2w ' ') || (w == c2w '\t')

sequenceHeader :: Parser (L.ByteString, L.ByteString)
sequenceHeader = do
  _ <- char (c2w '>')
  n <- takeWhile1P (Just "Name") isHeader
  _ <- takeWhileP (Just "Horizontal space") isHorizontalSpace
  d <- takeWhileP (Just "Description") isHeader
  _ <- eol
  return (n, d)

-- It is a little faster to directly pass the set of allowed characters. Then,
-- this set only has to be calculcated once per sequence in 'fastaSequence'.
sequenceLine :: S.Set Word8 -> Parser L.ByteString
sequenceLine s = do
  -- XXX: Will fail for non-capital letters.
  !xs <- takeWhile1P (Just "Alphabet character") (`S.member` s)
  _   <- void eol <|> eof
  return xs

-- XXX: If sequences are parsed line by line, the lines have to be copied when
-- forming the complete sequence. This is not memory efficient.

-- | Parse a sequence of characters.
fastaSequence :: Alphabet -> Parser Sequence
fastaSequence a = do
  (n, d) <- sequenceHeader
  let !alph = S.map toWord (A.all . alphabetSpec $ a)
  lns <- some (sequenceLine alph)
  _   <- many eol
  return $ Sequence n d a (fromByteString $ L.concat lns)

-- | Parse a Fasta file with given 'Alphabet'.
fasta :: Alphabet -> Parser [Sequence]
fasta a = some (fastaSequence a) <* eof
