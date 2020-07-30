-- |
-- Module      :  ELynx.Export.Sequence.Fasta
-- Description :  Export Fasta sequences
-- Copyright   :  (c) Dominik Schrempf 2018
-- License     :  GPL-3.0-or-later
--
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Write FASTA files.
--
-- [NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).
module ELynx.Export.Sequence.Fasta
  ( sequenceToFasta,
    sequencesToFasta,
  )
where

import qualified Data.ByteString.Lazy.Char8 as L
import ELynx.Data.Sequence.Sequence

fastaHeader :: L.ByteString -> L.ByteString -> L.ByteString
fastaHeader n d =
  L.singleton '>' <> n <> if L.null d then L.empty else L.pack " " <> d

-- | Convert a 'Sequence' to Fasta format.
sequenceToFasta :: Sequence -> L.ByteString
sequenceToFasta s =
  L.unlines [fastaHeader (name s) (description s), toByteString $ characters s]

-- | Convert a list 'Sequence's to Fasta format. A newline is added between any
-- two 'Sequence's.
sequencesToFasta :: [Sequence] -> L.ByteString
sequencesToFasta ss = L.concat $ map sequenceToFasta ss
