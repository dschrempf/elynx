{- |
Module      :  EvoMod.Export.Sequence.Fasta
Description :  Export Fasta sequences.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3


Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Write FASTA files.

[NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).

For more complicated parsers, try to use a [lexer](https://hackage.haskell.org/package/megaparsec-7.0.1/docs/Text-Megaparsec-Char-Lexer.html).
-}


module EvoMod.Export.Sequence.Fasta
  ( sequenceToFasta
  , sequencesToFasta
  ) where

import qualified Data.ByteString.Lazy.Char8                  as B

import           EvoMod.Data.Sequence.Sequence

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
