{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Module      :  EvoMod.Data.NucleotideX
Description :  Extended nucleotides including gaps and unknowns
Copyright   :  (c) Dominik Schrempf 2018

License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

See header of 'EvoMod.Data.Alphabet'.

Extended nucleotides with gaps and unknowns. See also
https://www.bioinformatics.org/sms/iupac.html or
https://en.wikipedia.org/wiki/International_Union_of_Pure_and_Applied_Chemistry.

@
Symbol  Description  Bases represented  Complement
------  -----------  -----------------  ----------
A       Adenine      A                  T
C       Cytosine        C               G
G       Guanine            G            C
T       Thymine               T         A
------  -----------  -----------------  ----------
N       any          A  C  G  T         N
------  -----------  -----------------  ----------
- or .  Gap (Zero)                      -
@


-}

module EvoMod.Data.Alphabet.NucleotideX
  ( NucleotideX (..)
  ) where

import qualified Data.Map.Strict                as M
import           Data.Vector.Unboxed.Deriving
import           Data.Word8

import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Tools.ByteString        (c2w)

-- | Extended nucleotides.
data NucleotideX = A | C | G | T
                 | N | Gap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWordM :: M.Map NucleotideX Word8
toWordM = M.fromList $ map (\(k, v) -> (k, c2w v))
  [ (A, 'A')
  , (C, 'C')
  , (G, 'G')
  , (T, 'T')
  , (N, 'N')
  , (Gap, '-')
  ]

toWord :: NucleotideX -> Word8
toWord = (M.!) toWordM

fromWordM :: M.Map Word8 NucleotideX
fromWordM = M.fromList $ map (\(k, v) -> (c2w k, v))
  [ ('A', A)
  , ('C', C)
  , ('G', G)
  , ('T', T)
  , ('N', N)
  , ('-', Gap)
  , ('.', Gap)                  -- Support dot gap character.
  ]

fromWord :: Word8 -> NucleotideX
fromWord = (M.!) fromWordM

derivingUnbox "NucleotideX"
    [t| NucleotideX -> Word8 |]
    [| toWord |]
    [| fromWord |]

instance C.Character NucleotideX where
  toWord   = toWord
  fromWord = fromWord
  codeName = "DNAX"
  codeNameVerbose = "DNAX (nucleotides; extended; including gaps and unknowns)"

toStandardM :: M.Map NucleotideX [NucleotideX]
toStandardM = M.fromList
  [
    (A, [A])
  , (C, [C])
  , (G, [G])
  , (T, [T])
  , (N, [A, C, G, T])
  , (Gap, [])
  ]

instance C.CharacterX NucleotideX where
  unknown    = N
  gap        = Gap
  toStandard = (M.!) toStandardM
