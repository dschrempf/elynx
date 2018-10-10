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
  ( AminoAcid (..)
  ) where

import           Data.Vector.Unboxed.Deriving
import           Data.Word8                   (Word8, toLower, toUpper)

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
aminoAcids = Alphabet $ map c2w [ 'A', 'C', 'D', 'E', 'F'
                                , 'G', 'H', 'I', 'K', 'L'
                                , 'M', 'N', 'P', 'Q', 'R'
                                , 'S', 'T', 'V', 'W', 'Y' ]

aminoAcids' :: Alphabet
aminoAcids' = Alphabet $ as ++ map toLower as
  where as = fromAlphabet aminoAcids

word8ToAminoAcid :: Word8 -> AminoAcid
word8ToAminoAcid w = if w' `elem` fromAlphabet aminoAcids
                      then AminoAcid w'
                      else error $ "Cannot read amino acid " ++ show w
  where w' = toUpper w

-- parseAminoAcidWord8 :: Parser Word8
-- parseAminoAcidWord8 = oneOf aminoAcids'

-- -- | Parse an amino acid.
-- parseAminoAcid :: Parser AminoAcid
-- parseAminoAcid = word8ToAminoAcid <$> parseAminoAcidWord8

instance Character AminoAcid where
  word8ToChar  = word8ToAminoAcid
  alphabet     = aminoAcids
  alphabet'    = aminoAcids'
  alphabetName = AA
