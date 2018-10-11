{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

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
  ( AminoAcid
  ) where

import qualified Data.Char                    as C
import qualified Data.Set                     as S
import           Data.Vector.Unboxed.Deriving
import           Data.Word8                   (Word8)

import           Evol.Data.Alphabet
import           Evol.Tools                   (c2w, w2c)

newtype AminoAcid = AminoAcid { fromAA :: Word8 }
  deriving (Eq)

instance Show AminoAcid where
  show (AminoAcid w) = [w2c w]

-- | Weird derivation of Unbox.
-- - [Question on Stack Overflow](https://stackoverflow.com/questions/10866676/how-do-i-write-a-data-vector-unboxed-instance-in-haskell)
-- - [GitHub issue](https://github.com/haskell/vector/issues/16)
-- - [Template Haskell deriving](http://hackage.haskell.org/package/vector-th-unbox)
derivingUnbox "AminoAcid"
    [t| AminoAcid -> Word8 |]
    [| \(AminoAcid x) -> x  |]
    [| AminoAcid |]

aminoAcids :: Alphabet
aminoAcids = Alphabet $ S.fromList [ 'A', 'C', 'D', 'E', 'F'
                                   , 'G', 'H', 'I', 'K', 'L'
                                   , 'M', 'N', 'P', 'Q', 'R'
                                   , 'S', 'T', 'V', 'W', 'Y' ]

aminoAcids' :: Alphabet
aminoAcids' = Alphabet $ as `S.union` S.map C.toLower as
  where as = fromAlphabet aminoAcids

-- | XXX: This is checked various times. E.g., during parsing.
charToAminoAcid :: Char -> AminoAcid
charToAminoAcid c = if c' `S.member` fromAlphabet aminoAcids
                      then AminoAcid $ c2w c'
                      else error $ "Cannot read amino acid " ++ show c
  where c' = C.toUpper c

-- | XXX: But unsafe conversion is not faster.
-- charToAminoAcidUnsafe :: Char -> AminoAcid
-- charToAminoAcidUnsafe c = AminoAcid $ c2w $ C.toUpper c

-- parseAminoAcidWord8 :: Parser Word8
-- parseAminoAcidWord8 = oneOf aminoAcids'

-- -- | Parse an amino acid.
-- parseAminoAcid :: Parser AminoAcid
-- parseAminoAcid = word8ToAminoAcid <$> parseAminoAcidWord8

instance Character AminoAcid where
  fromCharToAChar = charToAminoAcid
  fromACharToChar = w2c . fromAA
  alphabet        = aminoAcids
  alphabet'       = aminoAcids'
  alphabetName    = AA
