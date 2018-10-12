{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      :  Evol.Data.AminoAcid
Description :  Amino acid related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:26:35 2018.

-}


module Evol.Data.AminoAcid
  -- ( AminoAcid
  ( aminoAcids
  ) where

import qualified Data.Set                     as S
-- import           Data.Vector.Unboxed.Deriving
-- import           Data.Word8                   (Word8, toUpper)

import           Evol.Data.Alphabet
-- import           Evol.Tools                   (c2w, w2c)
import           Evol.Tools                   (c2w)

-- newtype AminoAcid = AminoAcid { fromAA :: Word8 }
--   deriving (Eq)

-- instance Show AminoAcid where
--   show (AminoAcid w) = [w2c w]

-- -- | Weird derivation of Unbox.
-- -- - [Question on Stack Overflow](https://stackoverflow.com/questions/10866676/how-do-i-write-a-data-vector-unboxed-instance-in-haskell)
-- -- - [GitHub issue](https://github.com/haskell/vector/issues/16)
-- -- - [Template Haskell deriving](http://hackage.haskell.org/package/vector-th-unbox)
-- derivingUnbox "AminoAcid"
--     [t| AminoAcid -> Word8 |]
--     [| \(AminoAcid x) -> x  |]
--     [| AminoAcid |]

aminoAcids :: Alphabet
aminoAcids = Alphabet . S.fromList $
  map c2w [ 'A', 'C', 'D', 'E', 'F'
          , 'G', 'H', 'I', 'K', 'L'
          , 'M', 'N', 'P', 'Q', 'R'
          , 'S', 'T', 'V', 'W', 'Y' ]

-- word8ToAminoAcid :: Word8 -> AminoAcid
-- word8ToAminoAcid w = if w' `S.member` fromAlphabet aminoAcids
--                       then AminoAcid w'
--                       else error $ "Cannot read amino acid " ++ show w
--   where w' = toUpper w

-- instance ACharacter AminoAcid where
--   fromWord8ToAChar = word8ToAminoAcid
--   fromACharToWord8 = fromAA
--   alphabetName    = AA

-- AA_IUPAC
-- Amino Acid Code:  Three letter Code:  Amino Acid:
-- ----------------  ------------------  -----------
-- A.................Ala.................Alanine
-- B.................Asx.................Aspartic acid or Asparagine
-- C.................Cys.................Cysteine
-- D.................Asp.................Aspartic Acid
-- E.................Glu.................Glutamic Acid
-- F.................Phe.................Phenylalanine
-- G.................Gly.................Glycine
-- H.................His.................Histidine
-- I.................Ile.................Isoleucine
-- K.................Lys.................Lysine
-- L.................Leu.................Leucine
-- M.................Met.................Methionine
-- N.................Asn.................Asparagine
-- P.................Pro.................Proline
-- Q.................Gln.................Glutamine
-- R.................Arg.................Arginine
-- S.................Ser.................Serine
-- T.................Thr.................Threonine
-- V.................Val.................Valine
-- W.................Trp.................Tryptophan
-- X.................Xaa.................Any amino acid
-- Y.................Tyr.................Tyrosine
-- Z.................Glx.................Glutamine or Glutamic acid
