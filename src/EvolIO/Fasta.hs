{- |
Module      :  Fasta
Description :  Fasta sequences.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:29:26 2018.

-}


module EvolIO.Fasta
  ( fasta
  , fastaNucleotide
  , fastaAminoAcid
  , sequenceName
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text

import           Base.Alphabet
import           Base.AminoAcid
import           Base.MultiSequenceAlignment
import           Base.Nucleotide
import           Base.Sequence


allowedChar :: Parser Char
allowedChar = digit <|> letter <|> char '_'

sequenceName :: Parser String
sequenceName = char '>' *> many1 allowedChar <* endOfLine

sequenceLine :: Alphabet a => Parser (Sequence a)
sequenceLine = parseSequence <* endOfLine

parseNamedSequence :: Alphabet a => Parser (NamedSequence a)
parseNamedSequence = do n  <- sequenceName
                        ss <- many1 sequenceLine
                        return (NamedSequence n (mconcat ss))

fasta :: Alphabet a => Parser (MultiSequenceAlignment a)
fasta = do nss <- many1 parseNamedSequence <* endOfInput
           let msa = MSA nss (length nss)
           if equalNrSites msa
             then return msa
             else error "Sequences do not have equal length."

fastaNucleotide :: Parser (MultiSequenceAlignment Nucleotide)
fastaNucleotide = fasta

fastaAminoAcid :: Parser (MultiSequenceAlignment AminoAcid)
fastaAminoAcid = fasta
