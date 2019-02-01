{- |
Module      :  EvoMod.Data.MarkovProcess.SubstitutionModel
Description :  Data type describing substitution model
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 19:10:46 2019.

-}

module EvoMod.Data.MarkovProcess.SubstitutionModel
  ( SubstitutionModel (..)
  , summarizeSubstitutionModel
  ) where

import qualified Data.ByteString.Lazy.Char8           as B

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.RateMatrix

-- | Complete definition of a substitution model.
data SubstitutionModel = SubstitutionModel
  { smCode                   :: Code
  , smName                   :: B.ByteString
  , smParams                 :: [Double]
  , smStationaryDistribution :: StationaryDistribution
  , smExchMatrix             :: ExchMatrix
  , smRateMatrix             :: RateMatrix
  }

-- | Summarize a substitution model; to be printed to screen or log.
summarizeSubstitutionModel :: SubstitutionModel -> B.ByteString
summarizeSubstitutionModel sm = B.unlines $ map B.pack
  [ "Substitution model."
  , "Code: " ++ show (smCode sm) ++ "."
  , "Name: " ++ show (smName sm) ++ "."
  , "Parameters: " ++ show (smParams sm) ++ "."
  , "Stationary frequencies: " ++ show (smStationaryDistribution sm) ++ "."
     -- XXX: This will be very verbose with amino acids or codons.
  , "Rate matrix: " ++ show (smRateMatrix sm) ++ "." ]
