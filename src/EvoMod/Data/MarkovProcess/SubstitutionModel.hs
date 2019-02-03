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

-- | Summarize a substitution model; lines to be printed to screen or log.
summarizeSubstitutionModel :: SubstitutionModel -> [B.ByteString]
summarizeSubstitutionModel sm = map B.pack $
  (show (smCode sm) ++ " substitution model: " ++ B.unpack (smName sm) ++ ".") :
  [ "Parameters: " ++ show (smParams sm) ++ "." | not (null (smParams sm))] ++
  case smCode sm of
    DNA -> [ "Stationary distribution: " ++ show (smStationaryDistribution sm) ++ "."
           , "Rate matrix: " ++ show (smRateMatrix sm) ++ "." ]
    Protein -> [ "Stationary distribution: " ++ show (smStationaryDistribution sm) ++ "." ]
    _ -> []
