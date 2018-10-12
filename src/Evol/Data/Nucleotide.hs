{- |
Module      :  Evol.Data.Nucleotide
Description :  Nucleotide related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

-}


module Evol.Data.Nucleotide
  -- ( Nucleotide
  -- , NucleotideIUPAC )
  ( nucleotides
  , nucleotidesIUPAC )
where

import qualified Data.Set                     as S
-- import           Data.Vector.Unboxed.Deriving
-- import           Data.Word8                   (Word8, toUpper)

import           Evol.Data.Alphabet
-- import           Evol.Tools                   (c2w, w2c)
import           Evol.Tools                   (c2w)

-- -- | Nucleotide data type. Actually, only two bits are needed, but 'Word8' is
-- -- the smallest available data type. One could use a 'pack' function like it is
-- -- done with 'ByteString's to decrease memory usage and pack a number of
-- -- 'Nucleotide's into one 'Word8'. By convention, I use uppercase letters.
-- newtype Nucleotide = Nucleotide { fromNuc :: Word8 }
--   deriving (Eq)

-- instance Show Nucleotide where
--   show (Nucleotide w) = [w2c w]

nucleotides :: Alphabet
nucleotides = Alphabet . S.fromList $ map c2w ['A', 'C', 'G', 'T']

-- word8ToNucleotide :: Word8 -> Nucleotide
-- word8ToNucleotide w = if w' `S.member` fromAlphabet nucleotides
--                       then Nucleotide w'
--                       else error $ "Cannot read nucleotide " ++ show w
--   where w' = toUpper w

-- instance ACharacter Nucleotide where
--   fromWord8ToAChar = word8ToNucleotide
--   fromACharToWord8 = fromNuc
--   alphabetName     = DNA

-- -- A  adenosine          C  cytidine             G  guanine
-- -- T  thymidine          N  A/G/C/T (any)        U  uridine
-- -- K  G/T (keto)         S  G/C (strong)         Y  T/C (pyrimidine)
-- -- M  A/C (amino)        W  A/T (weak)           R  G/A (purine)
-- -- B  G/T/C              D  G/A/T                H  A/C/T
-- -- V  G/C/A              -  gap of indeterminate length
-- newtype NucleotideIUPAC = NucleotideIUPAC { fromNucIUPAC :: Word8 }
--   deriving (Eq)

-- instance Show NucleotideIUPAC where
--   show (NucleotideIUPAC w) = [w2c w]

nucleotidesIUPAC :: Alphabet
nucleotidesIUPAC = Alphabet . S.fromList $
  map c2w [ 'A', 'C', 'G'
          , 'T', 'N', 'U'
          , 'K', 'S', 'Y'
          , 'M', 'W', 'R'
          , 'B', 'D', 'H'
          , 'V', '-']

-- word8ToNucleotideIUPAC :: Word8 -> NucleotideIUPAC
-- word8ToNucleotideIUPAC w = if w' `S.member` fromAlphabet nucleotidesIUPAC
--                            then NucleotideIUPAC w'
--                            else error $ "Cannot read nucleotide IUPAC code " ++ show w
--   where w' = toUpper w

-- instance ACharacter NucleotideIUPAC where
--   fromWord8ToAChar = word8ToNucleotideIUPAC
--   fromACharToWord8 = fromNucIUPAC
--   alphabetName     = DNA_IUPAC
