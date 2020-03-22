{- |
Module      :  ELynx.Data.Sequence.Translate
Description :  Translate sequences
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May 17 13:49:18 2019.

-}

module ELynx.Data.Sequence.Translate
  ( translateSeq
  )
where

import qualified Data.Vector.Unboxed           as V

import           ELynx.Data.Alphabet.Alphabet
import qualified ELynx.Data.Alphabet.Character as C
import           ELynx.Data.Character.Codon
import           ELynx.Data.Sequence.Sequence
import           ELynx.Tools

-- | Translate a sequence from 'DNA' or 'DNAX' to 'ProteinS'.
translateSeq :: UniversalCode -> Int -> Sequence -> Sequence
translateSeq uc rf (Sequence n d a cs) = case a of
  DNA  -> Sequence n d ProteinS (cs' $ translate uc)
  DNAX -> Sequence n d ProteinS (cs' $ translateX uc)
  DNAI -> Sequence n d ProteinI (cs' $ translateI uc)
  _    -> error "translate: can only translate DNA, DNAX, and DNAI."
  where cs' f = C.fromCVec $ translateVecWith f rf (C.toCVec cs)

-- Translate from DNA to Protein with given reading frame (0, 1, 2).
translateVecWith
  :: (V.Unbox a, Ord a, V.Unbox b)
  => (Codon a -> b)
  -> Int
  -> V.Vector a
  -> V.Vector b
translateVecWith f rf cs
  | rf > 2    = error "translateVecWith: reading frame is larger than 2."
  | rf < 0    = error "translateVecWith: reading frame is negative."
  | otherwise = aas
 where
  codons = map unsafeFromVec $ chopVec 3 $ V.drop rf cs
  aas    = V.fromList $ map f codons
