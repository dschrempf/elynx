-- |
-- Module      :  ELynx.Data.MarkovProcess.MixtureModel
-- Description :  Mixture models are a set of substitution models with weights
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Jan 29 19:17:40 2019.
--
-- To be imported qualified.
module ELynx.Data.MarkovProcess.MixtureModel
  ( -- * Types
    Weight,
    Component (weight, substModel),
    MixtureModel (name, alphabet, components),

    -- * Getters
    getWeights,
    getSubstitutionModels,

    -- * Building mixture models
    fromSubstitutionModels,

    -- * Transformations
    concatenate,
    scale,
    normalize,
    appendNameComponents,
  )
where

import qualified Data.List.NonEmpty as N
import Data.Semigroup
import ELynx.Data.Alphabet.Alphabet hiding (all)
import qualified ELynx.Data.MarkovProcess.SubstitutionModel as S
import Prelude

-- | Mixture model component weight.
type Weight = Double

-- | A mixture model component has a weight and a substitution model.
data Component = Component
  { weight :: Weight,
    substModel :: S.SubstitutionModel
  }
  deriving (Show, Read)

-- | A mixture model with its components.
data MixtureModel = MixtureModel
  { -- | Name
    name :: S.Name,
    alphabet :: Alphabet,
    components :: N.NonEmpty Component
  }
  deriving (Show, Read)

-- | Get weights.
getWeights :: MixtureModel -> N.NonEmpty Weight
getWeights = N.map weight . components

-- | Get substitution models.
getSubstitutionModels :: MixtureModel -> N.NonEmpty S.SubstitutionModel
getSubstitutionModels = N.map substModel . components

-- | Create a mixture model from a list of substitution models.
fromSubstitutionModels ::
  S.Name -> N.NonEmpty Weight -> N.NonEmpty S.SubstitutionModel -> MixtureModel
fromSubstitutionModels n ws sms =
  if allEqual $ N.toList alphs
    then MixtureModel n (N.head alphs) comps
    else
      error
        "fromSubstitutionModels: alphabets of substitution models are not equal."
  where
    comps = N.zipWith Component ws sms
    alphs = N.map S.alphabet sms
    allEqual [] = True
    allEqual xs = all (== head xs) $ tail xs

-- | Concatenate mixture models.
concatenate :: S.Name -> N.NonEmpty MixtureModel -> MixtureModel
concatenate n mms = fromSubstitutionModels n ws sms
  where
    comps = sconcat $ N.map components mms
    ws = N.map weight comps
    sms = N.map substModel comps

scaleComponent :: Double -> Component -> Component
scaleComponent s c = c {substModel = s'} where s' = S.scale s $ substModel c

-- | Scale all substitution models of the mixture model.
scale :: Double -> MixtureModel -> MixtureModel
scale s m = m {components = cs'}
  where
    cs = components m
    cs' = N.map (scaleComponent s) cs

-- | Globally normalize a mixture model so that on average one event happens per
-- unit time.
normalize :: MixtureModel -> MixtureModel
normalize mm = scale (1 / c) mm
  where
    c = sum $ N.zipWith (*) weights scales
    weights = getWeights mm
    scales = N.map S.totalRate $ getSubstitutionModels mm

appendNameComponent :: S.Name -> Component -> Component
appendNameComponent n c = c {substModel = s'}
  where
    s' = S.appendName n $ substModel c

-- | Append byte string to all substitution models of mixture model.
appendNameComponents :: S.Name -> MixtureModel -> MixtureModel
appendNameComponents n m = m {components = cs'}
  where
    cs = components m
    cs' = N.map (appendNameComponent n) cs
