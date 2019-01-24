{- |
Module      :  EvoMod.Data.RateMatrix.Nucleotide
Description :  Substitution models using nucleotides
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jan 24 08:33:26 2019.

XXX: Maybe rename to something like /DNA substitution models/. Nucleotide ~
Alphabet; DNA ~ Character.

-}

module EvoMod.Data.RateMatrix.Nucleotide
  ( jc
  , hky
  ) where

import Numeric.LinearAlgebra

import EvoMod.Data.RateMatrix.RateMatrix

jc :: Matrix R
jc = (4><4)
     [ 0.0, 1.0, 1.0, 1.0
     , 1.0, 0.0, 1.0, 1.0
     , 1.0, 1.0, 0.0, 1.0
     , 1.0, 1.0, 1.0, 0.0 ]

hky :: Double -> StationaryDist -> RateMatrix
hky k = fromExchMatrix em
  where em = (4><4)
             [ 0.0, 1.0,   k, 1.0
             , 1.0, 0.0, 1.0,   k
             ,   k, 1.0, 0.0, 1.0
             , 1.0,   k, 1.0, 0.0 ]


