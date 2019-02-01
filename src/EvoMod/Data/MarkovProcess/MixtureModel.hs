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
  , summarizeMixtureModelComponent
  , MixtureModel (..)
  , summarizeMixtureModel
  ) where

import qualified Data.ByteString.Builder                     as B
import qualified Data.ByteString.Lazy.Char8                  as B

import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | A mixture model component has a weight and a substitution model.
data MixtureModelComponent = MixtureModelComponent
  { mmcWeight            :: Double
  , mmcSubstitutionModel :: SubstitutionModel
  }

-- | Summarize a mixture model component; lines to be printed to screen or log.
summarizeMixtureModelComponent :: MixtureModelComponent -> [B.ByteString]
summarizeMixtureModelComponent mmc =
  B.pack "Weight: " <> (B.toLazyByteString . B.doubleDec $ mmcWeight mmc)
  : summarizeSubstitutionModel (mmcSubstitutionModel mmc)

-- | A mixture model with its components.
data MixtureModel = MixtureModel
  { mmName       :: B.ByteString
  , mmComponents :: [MixtureModelComponent]
  }

-- | Summarize a mixture model; lines to be printed to screen or log.
summarizeMixtureModel :: MixtureModel -> [B.ByteString]
summarizeMixtureModel mm =
  B.pack "Mixture model " <> mmName mm
  : concat [ B.pack ("Component " ++ show i ++ ":") : summarizeMixtureModelComponent c
            | (i, c) <- zip [0 :: Int ..] (mmComponents mm) ]
