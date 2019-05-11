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

-}


module EvoMod.Export.Sequence.Fasta
  ( sequenceToFasta
  , sequencesToFasta
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8    as L

import           EvoMod.Data.Sequence.Sequence

fastaHeader :: L.ByteString -> L.ByteString
fastaHeader i = L.singleton '>' <> i

-- | Convert a 'Sequence' to Fasta format.
sequenceToFasta :: Sequence -> L.ByteString
sequenceToFasta s = L.unlines [ fastaHeader $ s^.seqName , fromCharacters $ s^.seqCharacters ]

-- XXX: Remove newline between sequences.
-- | Convert a list 'Sequence's to Fasta format. A newline is added between any
-- two 'Sequence's.
sequencesToFasta :: [Sequence] -> L.ByteString
sequencesToFasta ss = L.unlines $ map sequenceToFasta ss
