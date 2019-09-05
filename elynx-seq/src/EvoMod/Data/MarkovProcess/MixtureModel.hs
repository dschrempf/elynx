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

To be imported qualified.

-}

module EvoMod.Data.MarkovProcess.MixtureModel
  ( Weight
  , Component (Component)
  , summarizeComponent
  , MixtureModel (MixtureModel)
  , name
  , fromSubstitutionModels
  , concatenate
  , summarize
  , isValid
  , getAlphabet
  , getWeights
  , getSubstitutionModels
  , scale
  , normalize
  , appendName
  ) where

import           Control.Lens
import qualified Data.ByteString.Builder                     as L
import qualified Data.ByteString.Lazy.Char8                  as L

import           EvoMod.Data.Alphabet.Alphabet
import qualified EvoMod.Data.MarkovProcess.SubstitutionModel as S
import           EvoMod.Tools.Equality                       (allEqual)

-- | Mixture model component weight.
type Weight = Double

-- | A mixture model component has a weight and a substitution model.
data Component = Component
  { _weight     :: Weight
  , _substModel :: S.SubstitutionModel
  }
  deriving (Show, Read)

makeLenses ''Component

-- | Summarize a mixture model component; lines to be printed to screen or log.
summarizeComponent :: Component -> [L.ByteString]
summarizeComponent mmc =
  L.pack "Weight: " <> (L.toLazyByteString . L.doubleDec $ mmc ^. weight)
  : S.summarize (mmc ^. substModel)

-- | A mixture model with its components.
data MixtureModel = MixtureModel
  { _name       :: S.Name
  , _components :: [Component]
  }
  deriving (Show, Read)

makeLenses ''MixtureModel

-- | Create a mixture model from a list of substitution models.
fromSubstitutionModels :: S.Name -> [Weight] -> [S.SubstitutionModel] -> MixtureModel
fromSubstitutionModels n ws sms = MixtureModel n comps
  where comps = zipWith Component ws sms

-- | Concatenate mixture models.
concatenate :: S.Name -> [MixtureModel] -> MixtureModel
concatenate n mms = MixtureModel n $ concatMap (view components) mms

-- | Summarize a mixture model; lines to be printed to screen or log.
summarize :: MixtureModel -> [L.ByteString]
summarize mm =
  L.pack ("Mixture model: " ++ mm ^. name ++ ".")
  : concat [ L.pack ("Component " ++ show i ++ ":") : summarizeComponent c
            | (i, c) <- zip [1 :: Int ..] (mm ^. components) ]

-- | Checks if a mixture model is valid.
--
-- XXX: For the future, a proper way of creating mixture models might be of
-- interest. For example, not exporting the constructor nor the record fields
-- and providing an algebraic way of creating mixture models (empty and
-- addComponent which performs necessary checks).
isValid :: MixtureModel -> Bool
isValid mm = not (null $ mm ^. components)
                         && allEqual alphabets
  where alphabets = mm ^.. components . traverse . substModel . S.alphabet

-- | Get alphabet used with mixture model. Throws error if components use different
-- 'Alphabet's.
getAlphabet :: MixtureModel -> Alphabet
getAlphabet mm = if isValid mm
            -- then S.alphabet . substModel $ head (components mm)
            then head $ mm ^.. components . traverse . substModel . S.alphabet
            else error "Mixture model is invalid."

-- | Get weights.
getWeights :: MixtureModel -> [Weight]
getWeights m = m ^.. components . traverse . weight

-- | Get substitution models.
getSubstitutionModels :: MixtureModel -> [S.SubstitutionModel]
getSubstitutionModels m = m ^.. components . traverse . substModel

-- | Scale all substitution models of the mixture model.
scale :: Double -> MixtureModel -> MixtureModel
scale s = over (components . traverse . substModel) (S.scale s)

-- | Globally normalize a mixture model so that on average one event happens per
-- unit time.
normalize :: MixtureModel -> MixtureModel
normalize mm = scale (1/c) mm
  where c = sum $ zipWith (*) weights scales
        weights = getWeights mm
        scales  = map S.totalRate $ getSubstitutionModels mm

-- | Append byte string to all substitution models of mixture model.
appendName :: S.Name -> MixtureModel -> MixtureModel
appendName n = over (components . traverse . substModel) (S.appendName n)
