{- |
Module      :  EvoMod.Data.MarkovProcess.MixtureModel
Description :  Mixture models are a set of substitution models with weights
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 19:17:40 2019.

-}

module EvoMod.Data.MarkovProcess.MixtureModel
  ( Weight
  , MixtureModelComponent (..)
  , summarizeMixtureModelComponent
  , MixtureModel (..)
  , summarizeMixtureModel
  , isValidMixtureModel
  , mmCode
  , getWeights
  , getRateMatrices
  ) where

import qualified Data.ByteString.Builder                     as L
import qualified Data.ByteString.Lazy.Char8                  as L

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Tools.Equality                       (allEqual)

-- | Mixture model component weight.
type Weight = Double

-- | A mixture model component has a weight and a substitution model.
data MixtureModelComponent = MixtureModelComponent
  { mmcWeight            :: Weight
  , mmcSubstitutionModel :: SubstitutionModel
  }

-- | Summarize a mixture model component; lines to be printed to screen or log.
summarizeMixtureModelComponent :: MixtureModelComponent -> [L.ByteString]
summarizeMixtureModelComponent mmc =
  L.pack "Weight: " <> (L.toLazyByteString . L.doubleDec $ mmcWeight mmc)
  : summarizeSubstitutionModel (mmcSubstitutionModel mmc)

-- | A mixture model with its components.
data MixtureModel = MixtureModel
  { mmName       :: L.ByteString
  , mmComponents :: [MixtureModelComponent]
  }

-- | Summarize a mixture model; lines to be printed to screen or log.
summarizeMixtureModel :: MixtureModel -> [L.ByteString]
summarizeMixtureModel mm =
  L.pack "Mixture model: " <> mmName mm <> L.pack "."
  : concat [ L.pack ("Component " ++ show i ++ ":") : summarizeMixtureModelComponent c
            | (i, c) <- zip [1 :: Int ..] (mmComponents mm) ]

-- | Checks if a mixture model is valid.
--
-- XXX: For the future, a proper way of creating mixture models might be of
-- interest. For example, not exporting the constructor nor the record fields
-- and providing an algebraic way of creating mixture models (empty and
-- addComponent which performs necessary checks).
isValidMixtureModel :: MixtureModel -> Bool
isValidMixtureModel mm = allEqual (map (smCode . mmcSubstitutionModel) cs)
                         && not (null cs)
  where cs = mmComponents mm

-- | Throws error if components use different 'Code's.
mmCode :: MixtureModel -> Code
mmCode mm = if isValidMixtureModel mm
            then smCode . mmcSubstitutionModel $ head (mmComponents mm)
            else error "Mixture model is invalid."

-- | Extract weights.
getWeights :: MixtureModel -> [Double]
getWeights mm = map mmcWeight $ mmComponents mm

-- | Extract rate matrices.
getRateMatrices :: MixtureModel -> [RateMatrix]
getRateMatrices mm = map (smRateMatrix . mmcSubstitutionModel) (mmComponents mm)

