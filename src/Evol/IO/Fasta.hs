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
  , fastaNucleotideIUPAC
  , fastaAminoAcid
  , fastaMSA
  , fastaMSANucleotide
  , fastaMSANucleotideIUPAC
  , fastaMSAAminoAcid
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy             as B (ByteString, concat,
                                                        unpack)
import qualified Data.Vector.Unboxed              as V
import           Data.Word                        (Word8)
import           Text.Megaparsec
import           Text.Megaparsec.Byte

import           Evol.Data.Alphabet
import           Evol.Data.AminoAcid
import           Evol.Data.MultiSequenceAlignment
import           Evol.Data.Nucleotide
import           Evol.Data.Sequence
import           Evol.Defaults
import           Evol.Tools                       (c2w, w2c)


allowedChar :: Parser Word8
allowedChar = alphaNumChar <|> oneOf (map c2w ['_', '|', '.'])

sequenceId :: Parser [Word8]
sequenceId = char (c2w '>') *> some allowedChar <* eol

sequenceLine :: [Word8] -> Parser B.ByteString
sequenceLine a = do xs <- takeWhile1P (Just "Sequence characters") (`elem` a)
                    _  <- void eol <|> eof
                    return xs

-- TODO: How can I get the alphabet here? This is all fishy.
parseSequence :: (Alphabet a, V.Unbox a) => Parser (Sequence String a)
parseSequence = do i  <- sequenceId
                   cs <- some (sequenceLine nucleotidesIUPAC')
                   let da = V.force . V.fromList . map word8ToChar .B.unpack .  B.concat $ cs
                       i' = map w2c i
                   return $ Sequence i' da

fasta :: (Alphabet a, V.Unbox a) => Parser [Sequence String a]
fasta = parseSequence `sepBy` many eol <* eof

fastaNucleotide :: Parser [Sequence String Nucleotide]
fastaNucleotide = fasta

fastaNucleotideIUPAC :: Parser [Sequence String NucleotideIUPAC]
fastaNucleotideIUPAC = fasta

fastaAminoAcid :: Parser [Sequence String AminoAcid]
fastaAminoAcid = fasta

fastaMSA :: (Alphabet a, V.Unbox a) => Parser (MultiSequenceAlignment String a)
fastaMSA = do ss <- fasta
              if equalLength ss
                then return $ MSA ss (length ss) (lengthSequence $ head ss)
                else error "Sequences do not have equal length."

fastaMSANucleotide :: Parser (MultiSequenceAlignment String Nucleotide)
fastaMSANucleotide = fastaMSA

fastaMSANucleotideIUPAC :: Parser (MultiSequenceAlignment String NucleotideIUPAC)
fastaMSANucleotideIUPAC = fastaMSA

fastaMSAAminoAcid :: Parser (MultiSequenceAlignment String AminoAcid)
fastaMSAAminoAcid = fastaMSA
