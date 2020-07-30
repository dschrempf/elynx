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
    Component,
    MixtureModel (name),

    -- * Getters
    getAlphabet,
    getWeights,
    getSubstitutionModels,

    -- * Building mixture models
    fromSubstitutionModels,

    -- * Transformations
    concatenate,
    scale,
    normalize,
    appendNameComponents,

    -- * Output
    summarizeComponent,
    summarize,
  )
where

import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List.NonEmpty hiding (zip)
import Data.Semigroup
import ELynx.Data.Alphabet.Alphabet
import qualified ELynx.Data.MarkovProcess.SubstitutionModel as S
import ELynx.Tools
import Prelude hiding
  ( head,
    length,
    map,
    zipWith,
  )

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
    components :: NonEmpty Component
  }
  deriving (Show, Read)

-- | Get alphabet used with mixture model. Throws error if components use
-- different 'Alphabet's.
getAlphabet :: MixtureModel -> Alphabet
getAlphabet = alphabet

-- | Get weights.
getWeights :: MixtureModel -> NonEmpty Weight
getWeights = map weight . components

-- | Get substitution models.
getSubstitutionModels :: MixtureModel -> NonEmpty S.SubstitutionModel
getSubstitutionModels = map substModel . components

-- | Create a mixture model from a list of substitution models.
fromSubstitutionModels ::
  S.Name -> NonEmpty Weight -> NonEmpty S.SubstitutionModel -> MixtureModel
fromSubstitutionModels n ws sms =
  if allEqual $ toList alphs
    then MixtureModel n (head alphs) comps
    else
      error
        "fromSubstitutionModels: alphabets of substitution models are not equal."
  where
    comps = zipWith Component ws sms
    alphs = map S.alphabet sms

-- | Concatenate mixture models.
concatenate :: S.Name -> NonEmpty MixtureModel -> MixtureModel
concatenate n mms = fromSubstitutionModels n ws sms
  where
    comps = sconcat $ map components mms
    ws = map weight comps
    sms = map substModel comps

scaleComponent :: Double -> Component -> Component
scaleComponent s c = c {substModel = s'} where s' = S.scale s $ substModel c

-- | Scale all substitution models of the mixture model.
scale :: Double -> MixtureModel -> MixtureModel
scale s m = m {components = cs'}
  where
    cs = components m
    cs' = map (scaleComponent s) cs

-- | Globally normalize a mixture model so that on average one event happens per
-- unit time.
normalize :: MixtureModel -> MixtureModel
normalize mm = scale (1 / c) mm
  where
    c = sum $ zipWith (*) weights scales
    weights = getWeights mm
    scales = map S.totalRate $ getSubstitutionModels mm

appendNameComponent :: S.Name -> Component -> Component
appendNameComponent n c = c {substModel = s'}
  where
    s' = S.appendName n $ substModel c

-- | Append byte string to all substitution models of mixture model.
appendNameComponents :: S.Name -> MixtureModel -> MixtureModel
appendNameComponents n m = m {components = cs'}
  where
    cs = components m
    cs' = map (appendNameComponent n) cs

-- | Summarize a mixture model component; lines to be printed to screen or log.
summarizeComponent :: Component -> [L.ByteString]
summarizeComponent c =
  L.pack "Weight: "
    <> (L.toLazyByteString . L.doubleDec $ weight c) :
  S.summarize (substModel c)

-- | Summarize a mixture model; lines to be printed to screen or log.
summarize :: MixtureModel -> [L.ByteString]
summarize m =
  [ L.pack $ "Mixture model: " ++ name m ++ ".",
    L.pack $ "Number of components: " ++ show n ++ "."
  ]
    ++ detail
  where
    n = length $ components m
    detail =
      if n <= 100
        then
          concat
            [ L.pack ("Component " ++ show i ++ ":") : summarizeComponent c
              | (i, c) <- zip [1 :: Int ..] (toList $ components m)
            ]
        else []
