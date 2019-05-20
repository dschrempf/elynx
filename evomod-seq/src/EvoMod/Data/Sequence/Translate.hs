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
  ( translate
  )
where

import qualified Data.Map                       as M
import qualified Data.Vector.Unboxed            as V

import           EvoMod.Data.Alphabet.Alphabet
import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Data.Character.Codon
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Tools.Vector

-- | Translate a sequence from 'DNA' or 'DNAX' to 'ProteinS'.
translate :: UniversalCode -> Int -> Sequence -> Sequence
translate uc rf (Sequence n a cs) = case a of
                                      DNA  -> Sequence n ProteinS (cs' universalCode)
                                      DNAX -> Sequence n ProteinS (cs' universalCodeX)
                                      _    -> error "translate: can only translate DNA and DNAX."
  where cs' toUC = C.fromCVec $ translateWith (toUC uc) rf (C.toCVec cs)

-- | Translate from DNA to Protein with given reading frame (0, 1, 2).
translateWith :: (V.Unbox a, Ord a, V.Unbox b)
              => M.Map (Codon a) b -> Int -> V.Vector a -> V.Vector b
translateWith m rf cs | rf > 2    = error "translate: reading frame is larger than 2."
                      | rf < 0    = error "translate: reading frame is negative."
                      | otherwise  = aas
  where codons = map unsafeFromVec $ chop 3 $ V.drop rf cs
        aas = V.fromList $ map (m M.!) codons
