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
  , substitutionModel
  , scaleSubstitutionModel
  , renameSubstitutionModel
  , appendNameSubstitutionModel
  , summarizeSubstitutionModel
  ) where

import qualified Data.ByteString.Lazy.Char8           as L
import           Numeric.LinearAlgebra                hiding ((<>))

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.RateMatrix

-- | Complete definition of a substitution model.
data SubstitutionModel = SubstitutionModel
  { smCode                   :: Code
  , smName                   :: L.ByteString
  , smParams                 :: [Double]
  , smStationaryDistribution :: StationaryDistribution
  , smExchMatrix             :: ExchMatrix
  , smRateMatrix             :: RateMatrix
  }

-- | Create a substitution model.
substitutionModel :: Code
                  -> L.ByteString
                  -> [Double]
                  -> StationaryDistribution
                  -> ExchMatrix
                  -> SubstitutionModel
substitutionModel c n ps d e = SubstitutionModel c n ps d e (fromExchMatrix e d)

-- | Scale the rate of a substitution model by given factor.
scaleSubstitutionModel :: SubstitutionModel -> Double -> SubstitutionModel
scaleSubstitutionModel sm@(SubstitutionModel _ _ _ _ e r) s = sm { smExchMatrix = scale s e
                                                                 , smRateMatrix = scale s r }

-- | Rename a substitution model.
renameSubstitutionModel :: SubstitutionModel -> L.ByteString -> SubstitutionModel
renameSubstitutionModel sm n = sm { smName = n }

-- | Abbend to name.
appendNameSubstitutionModel :: SubstitutionModel -> L.ByteString -> SubstitutionModel
appendNameSubstitutionModel sm a = sm { smName = n'}
  where n' = smName sm <> a

-- | Summarize a substitution model; lines to be printed to screen or log.
summarizeSubstitutionModel :: SubstitutionModel -> [L.ByteString]
summarizeSubstitutionModel sm = map L.pack $
  (show (smCode sm) ++ " substitution model: " ++ L.unpack (smName sm) ++ ".") :
  [ "Parameters: " ++ show (smParams sm) ++ "." | not (null (smParams sm))] ++
  case smCode sm of
    DNA -> [ "Stationary distribution: " ++ show (smStationaryDistribution sm) ++ "."
           , "Rate matrix: " ++ show (smRateMatrix sm) ++ "." ]
    Protein -> [ "Stationary distribution: " ++ show (smStationaryDistribution sm) ++ "." ]
    _ -> []
