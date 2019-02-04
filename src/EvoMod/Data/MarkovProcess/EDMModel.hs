{- |
Module      :  EvoMod.Data.MarkovProcess.EDMModel
Description :  Empirical distribution mixture models
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 19:57:55 2019.

-- TODO: Remove this model. It is not needed. An EDM model is just a normal
-- mixture model. What may be needed is a parser for EDM models.

Empricial distribution mixture (EDM) models are mixture models that share the
same exchangeability matrix but have different stationary distributions obtained
from data.

-}

module EvoMod.Data.MarkovProcess.EDMModel
  ( EDMComponent (..)
  , summarizeEDMComponents
  ) where

import qualified Data.ByteString.Lazy.Char8                  as B

import           EvoMod.Data.MarkovProcess.RateMatrix

-- | Empirical distribution mixture model component.
data EDMComponent = EDMComponent
  { cWeight                 :: Double
  , cStationaryDistribution :: StationaryDistribution
  }
  deriving (Show, Eq)

-- | Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> B.ByteString
summarizeEDMComponents cs = B.pack
                            $ "Empiricial distribution mixture model with "
                            ++ show (length cs) ++ " components"
