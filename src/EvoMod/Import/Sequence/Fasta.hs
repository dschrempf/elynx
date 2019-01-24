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
  , fastaFile
  , fastaFileMSA
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8                  as B
import qualified Data.Set                                    as S
import           Data.Void
import           Data.Word8                                  (Word8, toUpper)
import           Text.Megaparsec
import           Text.Megaparsec.Byte

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools                                (c2w, w2c)

-- | A shortcut.
type Parser = Parsec Void B.ByteString

allowedHeaderChar :: Parser Word8
allowedHeaderChar = alphaNumChar <|> oneOf (map c2w ['_', '|', '.'])

sequenceHeader :: Parser [Word8]
sequenceHeader = char (c2w '>') *> some allowedHeaderChar <* eol

checkWord8 :: Alphabet -> Word8 -> Bool
checkWord8 a w = toUpper w `S.member` fromAlphabet a

sequenceLine :: Alphabet -> Parser B.ByteString
sequenceLine a = do xs <- takeWhile1P (Just "Alphabet character") (checkWord8 a)
                    _  <- void eol <|> eof
                    return xs

-- | Parse a sequence of 'Alphabet' 'EvoMod.Data.Alphabet.Character's.
fastaSequence :: Alphabet -> Parser Sequence
fastaSequence a = do hd <- sequenceHeader
                     cs <- some (sequenceLine a)
                     _  <- many eol
                     let hd' = B.pack $ map w2c hd
                     return $ toSequence hd' (B.concat cs)

-- TODO: Rename this function to plain 'fasta'.
-- | Parse a Fasta file assuming 'Code'.
fastaFile :: Code -> Parser [Sequence]
fastaFile c = some (fastaSequence (alphabet c)) <* eof

-- TODO: Remove this function.
-- | Parse a 'MultiSequenceAlignment' Fasta files assuming 'Code'.
fastaFileMSA :: Code -> Parser MultiSequenceAlignment
fastaFileMSA c = fromSequenceList <$> fastaFile c
