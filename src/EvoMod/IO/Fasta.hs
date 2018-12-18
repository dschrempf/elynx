{- |
Module      :  EvoMod.IO.Fasta
Description :  Fasta sequences.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3


Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Parse FASTA files.

[NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).

For more complicated parsers, try to use a [lexer](https://hackage.haskell.org/package/megaparsec-7.0.1/docs/Text-Megaparsec-Char-Lexer.html).
-}


module EvoMod.IO.Fasta
  ( Parser
  , fastaSequence
  , fastaFile
  , fastaFileMSA
  , sequenceToFasta
  , sequencesToFasta
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8       as B
import qualified Data.Set                         as S
import           Data.Void
import           Data.Word8                       (Word8, toUpper)
import           Text.Megaparsec
import           Text.Megaparsec.Byte

import           EvoMod.Data.Alphabet
import           EvoMod.Data.MultiSequenceAlignment
import           EvoMod.Data.Sequence
import           EvoMod.Tools

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

-- | Parse a sequence of 'Alphabet' 'Character's.
fastaSequence :: Alphabet -> Parser Sequence
fastaSequence a = do hd <- sequenceHeader
                     cs <- some (sequenceLine a)
                     _  <- many eol
                     let hd' = map w2c hd
                     return $ toSequence hd' (B.concat cs)

-- | Parse a Fasta file assuming 'Code'.
fastaFile :: Code -> Parser [Sequence]
fastaFile c = some (fastaSequence (alphabet c)) <* eof

-- | Parse a 'MultiSequenceAlignment' Fasta files assuming 'Code'.
fastaFileMSA :: Code -> Parser MultiSequenceAlignment
fastaFileMSA c = fromSequenceList <$> fastaFile c

fastaHeader :: String -> B.ByteString
fastaHeader i = B.pack $ '>' : i

-- | Convert a 'Sequence' to Fasta format.
sequenceToFasta :: Sequence -> B.ByteString
sequenceToFasta s = B.unlines [ fastaHeader i , cs ]
  where (i, cs) = fromSequence s

-- | Convert a list 'Sequence's to Fasta format. A newline is added between any
-- two 'Sequence's.
sequencesToFasta :: [Sequence] -> B.ByteString
sequencesToFasta ss = B.unlines $ map sequenceToFasta ss
