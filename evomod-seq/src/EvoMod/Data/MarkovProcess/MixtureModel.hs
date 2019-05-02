{-# LANGUAGE TemplateHaskell #-}

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
  ( MixtureModelName
  , Weight
  , MixtureModelComponent (MixtureModelComponent)
  , summarizeMixtureModelComponent
  , MixtureModel (MixtureModel)
  , mmName
  , fromSubstitutionModels
  , concatenateMixtureModels
  , summarizeMixtureModel
  , isValidMixtureModel
  , getCodeMixtureModel
  , getWeights
  , getSubstitutionModels
  , scaleMixtureModel
  , normalizeMixtureModel
  , appendNameMixtureModel
  ) where

import           Control.Lens
import qualified Data.ByteString.Builder                     as L
import qualified Data.ByteString.Lazy.Char8                  as L

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Tools.Equality                       (allEqual)

-- | Mixture model component weight.
type Weight = Double

-- | A mixture model component has a weight and a substitution model.
data MixtureModelComponent = MixtureModelComponent
  { _weight     :: Weight
  , _substModel :: SubstitutionModel
  }
  deriving (Show, Read)

makeLenses ''MixtureModelComponent

-- | Summarize a mixture model component; lines to be printed to screen or log.
summarizeMixtureModelComponent :: MixtureModelComponent -> [L.ByteString]
summarizeMixtureModelComponent mmc =
  L.pack "Weight: " <> (L.toLazyByteString . L.doubleDec $ mmc ^. weight)
  : summarizeSubstitutionModel (mmc ^. substModel)

-- | Abstracted because may change in the future.
type MixtureModelName = SubstitutionModelName

-- | A mixture model with its components.
data MixtureModel = MixtureModel
  { _name       :: MixtureModelName
  , _components :: [MixtureModelComponent]
  }
  deriving (Show, Read)

makeLenses ''MixtureModel

-- | Access name.
mmName :: Lens' MixtureModel MixtureModelName
mmName = name

-- | Create a mixture model from a list of substitution models.
fromSubstitutionModels :: MixtureModelName -> [Weight] -> [SubstitutionModel] -> MixtureModel
fromSubstitutionModels n ws sms = MixtureModel n comps
  where comps = zipWith MixtureModelComponent ws sms

-- | Concatenate mixture models.
concatenateMixtureModels :: MixtureModelName -> [MixtureModel] -> MixtureModel
concatenateMixtureModels n mms = MixtureModel n $ concatMap (view components) mms

-- | Summarize a mixture model; lines to be printed to screen or log.
summarizeMixtureModel :: MixtureModel -> [L.ByteString]
summarizeMixtureModel mm =
  L.pack "Mixture model: " <> mm ^. name <> L.pack "."
  : concat [ L.pack ("Component " ++ show i ++ ":") : summarizeMixtureModelComponent c
            | (i, c) <- zip [1 :: Int ..] (mm ^. components) ]

-- | Checks if a mixture model is valid.
--
-- XXX: For the future, a proper way of creating mixture models might be of
-- interest. For example, not exporting the constructor nor the record fields
-- and providing an algebraic way of creating mixture models (empty and
-- addComponent which performs necessary checks).
isValidMixtureModel :: MixtureModel -> Bool
isValidMixtureModel mm = not (null $ mm ^. components)
                         && allEqual codes
  where codes = mm ^.. components . traverse . substModel . smCode

-- | Get code used with mixture model. Throws error if components use different
-- 'Code's.
getCodeMixtureModel :: MixtureModel -> Code
getCodeMixtureModel mm = if isValidMixtureModel mm
            -- then smCode . substModel $ head (components mm)
            then head $ mm ^.. components . traverse . substModel . smCode
            else error "Mixture model is invalid."

-- | Get weights.
getWeights :: MixtureModel -> [Weight]
getWeights m = m ^.. components . traverse . weight

-- | Get substitution models.
getSubstitutionModels :: MixtureModel -> [SubstitutionModel]
getSubstitutionModels m = m ^.. components . traverse . substModel

-- | Scale all substitution models of the mixture model.
scaleMixtureModel :: Double -> MixtureModel -> MixtureModel
scaleMixtureModel s = over (components . traverse . substModel) (scaleSubstitutionModel s)

-- | Globally normalize a mixture model so that on average one event happens per
-- unit time.
normalizeMixtureModel :: MixtureModel -> MixtureModel
normalizeMixtureModel mm = scaleMixtureModel (1/c) mm
  where c = sum $ zipWith (*) weights scales
        weights = getWeights mm
        scales  = map totalRateSubstitutionModel $ getSubstitutionModels mm

-- | Append byte string to all substitution models of mixture model.
appendNameMixtureModel :: MixtureModelName -> MixtureModel -> MixtureModel
appendNameMixtureModel n = over (components . traverse . substModel) (appendNameSubstitutionModel n)
