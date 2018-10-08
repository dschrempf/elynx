{- |
Module      :  Evol.IO.Fasta
Description :  Fasta sequences.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:29:26 2018.

Parse FASTA files.

[NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).

For more complicated parsers, try to use a [lexer](https://hackage.haskell.org/package/megaparsec-7.0.1/docs/Text-Megaparsec-Char-Lexer.html).
-}


module Evol.IO.Fasta
  ( fasta
  , fastaNucleotide
  , fastaAminoAcid
  , fastaMSA
  , fastaMSANucleotide
  , fastaMSAAminoAcid
  , sequenceName
  ) where

import           Control.Monad
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Evol.Data.Alphabet
import           Evol.Data.AminoAcid
import           Evol.Data.Defaults
import           Evol.Data.MultiSequenceAlignment
import           Evol.Data.Nucleotide
import           Evol.Data.Sequence


allowedChar :: Parser Char
allowedChar = alphaNumChar <|> char '_'

sequenceName :: Parser String
sequenceName = char '>' *> some allowedChar <* eol

sequenceLine :: Alphabet a => Parser (Sequence a)
-- Make sure that both 'eol' and 'eof' are accepted. The function 'void' is
-- needed so that the type check succeeds. Since the value is thrown away
-- anyways it should not make a difference.
sequenceLine = parseSequence <* (void eol <|> eof)

parseNamedSequence :: Alphabet a => Parser (NamedSequence a)
parseNamedSequence = do n  <- sequenceName
                        ss <- some sequenceLine
                        return (NamedSequence n (mconcat ss))

fasta :: Alphabet a => Parser [NamedSequence a]
fasta = some parseNamedSequence <* eof

fastaNucleotide :: Parser [NamedSequence Nucleotide]
fastaNucleotide = fasta

fastaAminoAcid :: Parser [NamedSequence AminoAcid]
fastaAminoAcid = fasta

fastaMSA :: Alphabet a => Parser (MultiSequenceAlignment a)
fastaMSA = do nss <- fasta
              if equalLengthNamedSequence nss
                then return $ MSA nss (length nss) (lengthNamedSequence $ head nss)
                else error "Sequences do not have equal length."

fastaMSANucleotide :: Parser (MultiSequenceAlignment Nucleotide)
fastaMSANucleotide = fastaMSA

fastaMSAAminoAcid :: Parser (MultiSequenceAlignment AminoAcid)
fastaMSAAminoAcid = fastaMSA
