{-# LANGUAGE BangPatterns #-}

{- |
Module      :  EvoMod.Import.Sequence.Fasta
Description :  Import Fasta sequences.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3


Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Parse FASTA files.

[NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).

For more complicated parsers, try to use a [lexer](https://hackage.haskell.org/package/megaparsec-7.0.1/docs/Text-Megaparsec-Byte-Lexer.html).
-}


module EvoMod.Import.Sequence.Fasta
  ( Parser
  , fastaSequence
  , fasta
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.Set                      as S
import           Data.Void
import           Data.Word8
import           Text.Megaparsec
import           Text.Megaparsec.Byte

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools.ByteString       (c2w)

-- | Shortcut.
type Parser = Parsec Void L.ByteString

isSpecial :: Word8 -> Bool
isSpecial w = w `elem` (map c2w ['_', '|', '.', '-'])

isHeaderChar :: Word8 -> Bool
isHeaderChar w = isAlphaNum w || isSpecial w

sequenceHeader :: Parser L.ByteString
sequenceHeader = do
  _ <- char (c2w '>')
  h <- takeWhile1P (Just "Header character") isHeaderChar
  -- XXX: Allow description.
  _ <- eol
  return h

-- It is a little faster to directly pass the set of allowed characters. Then,
-- this set only has to be calculcated once per sequence in 'fastaSequence'.
sequenceLine :: S.Set Word8 -> Parser L.ByteString
sequenceLine s = do
  !xs <- takeWhile1P (Just "Alphabet character") (`S.member` s)
  _  <- void eol <|> eof
  return xs

-- XXX: If sequences are parsed line by line, the lines have to be copied when
-- forming the complete sequence. This is not memory efficient.

-- | Parse a sequence of 'Alphabet' 'EvoMod.Data.Alphabet.Character's.
fastaSequence :: Code -> Parser Sequence
fastaSequence c = do hd <- sequenceHeader
                     let !a  = fromAlphabetLookup $ alphabetLookup c
                     lns <- some (sequenceLine a)
                     _  <- many eol
                     return $ Sequence hd c (L.concat lns)

-- | Parse a Fasta file assuming 'Code'.
fasta :: Code -> Parser [Sequence]
fasta c = some (fastaSequence c) <* eof
