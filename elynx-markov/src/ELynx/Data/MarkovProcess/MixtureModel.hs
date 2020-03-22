{- |
Module      :  ELynx.Data.MarkovProcess.MixtureModel
Description :  Mixture models are a set of substitution models with weights
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 19:17:40 2019.

To be imported qualified.

-}

module ELynx.Data.MarkovProcess.MixtureModel
  ( -- * Types
    Weight
  , Component(Component)
  , MixtureModel(MixtureModel)
  , -- * Getters
    name
  , getAlphabet
  , getWeights
  , getSubstitutionModels
    -- * Building mixture models
  , fromSubstitutionModels
    -- * Transformations
  , concatenate
  , scale
  , normalize
  , appendName
  -- * Tests
  , isValid
  -- * Output
  , summarizeComponent
  , summarize
  )
where

import qualified Data.ByteString.Builder       as L
import qualified Data.ByteString.Lazy.Char8    as L

import           ELynx.Data.Alphabet.Alphabet

import           ELynx.Tools

import qualified ELynx.Data.MarkovProcess.SubstitutionModel
                                               as S

-- | Mixture model component weight.
type Weight = Double

-- | A mixture model component has a weight and a substitution model.
data Component = Component
  { weight     :: Weight
  , substModel :: S.SubstitutionModel
  }
  deriving (Show, Read)

-- | A mixture model with its components.
data MixtureModel = MixtureModel
  { name       :: S.Name        -- ^ Name
  , components :: [Component]
  }
  deriving (Show, Read)

-- | Get alphabet used with mixture model. Throws error if components use
-- different 'Alphabet's.
getAlphabet :: MixtureModel -> Alphabet
getAlphabet mm = if isValid mm
  then S.alphabet . substModel $ head (components mm)
  else error "Mixture model is invalid."

-- | Get weights.
getWeights :: MixtureModel -> [Weight]
getWeights = map weight . components

-- | Get substitution models.
getSubstitutionModels :: MixtureModel -> [S.SubstitutionModel]
getSubstitutionModels = map substModel . components


-- | Create a mixture model from a list of substitution models.
fromSubstitutionModels
  :: S.Name -> [Weight] -> [S.SubstitutionModel] -> MixtureModel
fromSubstitutionModels n ws sms = MixtureModel n comps
  where comps = zipWith Component ws sms

-- | Concatenate mixture models.
concatenate :: S.Name -> [MixtureModel] -> MixtureModel
concatenate n mms = MixtureModel n $ concatMap components mms

scaleComponent :: Double -> Component -> Component
scaleComponent s c = c { substModel = s' } where s' = S.scale s $ substModel c

-- | Scale all substitution models of the mixture model.
scale :: Double -> MixtureModel -> MixtureModel
scale s m = m { components = cs' }
 where
  cs  = components m
  cs' = map (scaleComponent s) cs

-- | Globally normalize a mixture model so that on average one event happens per
-- unit time.
normalize :: MixtureModel -> MixtureModel
normalize mm = scale (1 / c) mm
 where
  c       = sum $ zipWith (*) weights scales
  weights = getWeights mm
  scales  = map S.totalRate $ getSubstitutionModels mm

appendNameComponent :: S.Name -> Component -> Component
appendNameComponent n c = c { substModel = s' }
  where s' = S.appendName n $ substModel c

-- | Append byte string to all substitution models of mixture model.
appendName :: S.Name -> MixtureModel -> MixtureModel
appendName n m = m { components = cs' }
 where
  cs  = components m
  cs' = map (appendNameComponent n) cs

-- | Checks if a mixture model is valid.
--
-- XXX: For the future, a proper way of creating mixture models might be of
-- interest. For example, not exporting the constructor nor the record fields
-- and providing an algebraic way of creating mixture models (empty and
-- addComponent which performs necessary checks).
isValid :: MixtureModel -> Bool
isValid m = not (null $ components m) && allEqual alphabets
  where alphabets = map (S.alphabet . substModel) $ components m

-- | Summarize a mixture model component; lines to be printed to screen or log.
summarizeComponent :: Component -> [L.ByteString]
summarizeComponent c =
  L.pack "Weight: "
    <> (L.toLazyByteString . L.doubleDec $ weight c)
    :  S.summarize (substModel c)

-- | Summarize a mixture model; lines to be printed to screen or log.
summarize :: MixtureModel -> [L.ByteString]
summarize m =
  [ L.pack $ "Mixture model: " ++ name m ++ "."
    , L.pack $ "Number of components: " ++ show n ++ "."
    ]
    ++ detail
 where
  n      = length $ components m
  detail = if n <= 100
    then concat
      [ L.pack ("Component " ++ show i ++ ":") : summarizeComponent c
      | (i, c) <- zip [1 :: Int ..] (components m)
      ]
    else []
