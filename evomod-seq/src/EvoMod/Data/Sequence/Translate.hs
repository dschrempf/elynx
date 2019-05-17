{- |
Module      :  EvoMod.Data.Sequence.Translate
Description :  Translate sequences
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May 17 13:49:18 2019.

-}

module EvoMod.Data.Sequence.Translate
  ( translateDNA
  , translateDNAX
  ) where

import qualified Data.Map                         as M
import qualified Data.Vector.Unboxed              as V

import           EvoMod.Data.Alphabet.AminoAcidS
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Alphabet.Codon
import           EvoMod.Data.Alphabet.Nucleotide
import           EvoMod.Data.Alphabet.NucleotideX
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools.List

-- TODO: This function goes via lists. Super slow.
-- | Translate from DNA to Protein with given reading frame (0, 1, 2).
translateWith :: (Character a, Character b)
              => (M.Map (Codon a) b) -> Int -> Sequence a -> Sequence b
translateWith m rf (Sequence n cs) | rf > 2    = error "translate: reading frame is larger than 2."
                                   | rf < 0    = error "translate: reading frame is negative."
                                   | otherwise  = Sequence n aas
  where codons = map Codon $ chop3 $ V.toList $ V.drop rf cs
        aas = V.fromList $ map (m M.!) codons

translateDNA :: UniversalCode -> Int -> Sequence Nucleotide -> Sequence AminoAcidS
translateDNA uc = translateWith (universalCode uc)

translateDNAX :: UniversalCode -> Int -> Sequence NucleotideX -> Sequence AminoAcidS
translateDNAX uc = translateWith (universalCodeX uc)
