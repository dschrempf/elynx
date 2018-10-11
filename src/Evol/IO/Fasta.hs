{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
  ( fastaSequence
  , fastaFile
  , fastaFileMSA
  , sequenceToFasta
  , sequencesToFasta
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8       as B
import qualified Data.Set                         as S
import qualified Data.Vector.Unboxed              as V
import           Data.Word                        (Word8)
import           Text.Megaparsec
import           Text.Megaparsec.Byte

import           Evol.Data.Alphabet
import           Evol.Data.MultiSequenceAlignment
import           Evol.Data.Sequence
import           Evol.Defaults
import           Evol.Tools


allowedHeaderChar :: Parser Word8
allowedHeaderChar = alphaNumChar <|> oneOf (map c2w ['_', '|', '.'])

sequenceHeader :: Parser [Word8]
sequenceHeader = char (c2w '>') *> some allowedHeaderChar <* eol

sequenceLine :: Alphabet -> Parser B.ByteString
sequenceLine a = do xs <- takeWhile1P (Just "Alphabet characters") (\x -> w2c x `S.member` fromAlphabet a)
                    _  <- void eol <|> eof
                    return xs

fastaSequence :: forall a . (Character a, V.Unbox a) => Parser (Sequence String a)
fastaSequence = do i  <- sequenceHeader
                   cs <- some (sequenceLine $ alphabet' @a)
                   _  <- many eol
                   let bs = B.concat cs
                       -- TODO: Use V.generate. But Int cs. Int64?
                       -- l  = fromIntegral $ B.length bs :: Int
                       -- v  = V.generate (l) $ \i -> fromCharToAChar (bs `B.index` i)
                       da = V.force . V.fromList . map fromCharToAChar . B.unpack $ bs
                       i' = map w2c i
                   return $ Sequence i' da

fastaFile :: (Character a, V.Unbox a) => Parser [Sequence String a]
fastaFile = some fastaSequence <* eof

fastaFileMSA :: (Character a, V.Unbox a) => Parser (MultiSequenceAlignment String a)
fastaFileMSA = do ss <- fastaFile
                  if equalLength ss
                    then return $ MSA ss (length ss) (lengthSequence $ head ss)
                    else error "Sequences do not have equal length."

fastaHeader :: (Show i) => Sequence i a -> B.ByteString
fastaHeader s = B.pack $ '>' : showWithoutQuotes (seqId s)

fastaBody :: (Show a, Character a, V.Unbox a) => Sequence i a -> B.ByteString
fastaBody s = B.pack . V.toList . V.map fromACharToChar $ seqCs s

sequenceToFasta :: (Show i, Show a, Character a, V.Unbox a) => Sequence i a -> B.ByteString
sequenceToFasta s = B.unlines [fastaHeader s, fastaBody s]

sequencesToFasta :: (Show i, Show a, Character a, V.Unbox a) => [Sequence i a] -> B.ByteString
sequencesToFasta ss = B.unlines $ map sequenceToFasta ss
