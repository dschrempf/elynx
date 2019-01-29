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
  ( MixtureModelComponent (..)
  , MixtureModel (..)
  ) where

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | A mixture model component has a weight and a substitution model.
data MixtureModelComponent = MixtureModelComponent
  { mmcWeight            :: Double
  , mmcSubstitutionModel :: SubstitutionModel
  }

-- | A mixture model with its components.
data MixtureModel = MixtureModel
  { mmCode       :: Code
  , mmName       :: String
  , mmComponents :: [MixtureModelComponent]
  }
