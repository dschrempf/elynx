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
import           Data.Void
import           Data.Word                     (Word8)
import           Text.Megaparsec
import           Text.Megaparsec.Byte

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools.ByteString       (c2w, w2c)

-- | Shortcut.
type Parser = Parsec Void L.ByteString

allowedHeaderChar :: Parser Word8
allowedHeaderChar = alphaNumChar <|> oneOf (map c2w ['_', '|', '.'])

sequenceHeader :: Parser [Word8]
sequenceHeader = char (c2w '>') *> some allowedHeaderChar <* eol

sequenceLine :: Code -> Parser L.ByteString
sequenceLine code = do
  xs <- takeWhile1P (Just "Alphabet character") (inAlphabet code)
  _  <- void eol <|> eof
  return xs

-- | Parse a sequence of 'Alphabet' 'EvoMod.Data.Alphabet.Character's.
fastaSequence :: Code -> Parser Sequence
fastaSequence code = do hd <- sequenceHeader
                        cs <- some (sequenceLine code)
                        _  <- many eol
                        let hd' = L.pack $ map w2c hd
                        return $ toSequence hd' code (L.concat cs)

-- | Parse a Fasta file assuming 'Code'.
fasta :: Code -> Parser [Sequence]
fasta c = some (fastaSequence c) <* eof
