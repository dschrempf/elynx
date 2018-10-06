{- |
Module      :  Defaults
Description :  Various default values.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 23:00:17 2018.

-}


module Base.Defaults
  ( Parser
  , sequenceNameLength
  , sequenceSummaryLength
  ) where

import           Data.Text
import           Data.Void
import           Text.Megaparsec

type Parser = Parsec Void Text

-- | Space reserved for sequence names when printing them.
sequenceNameLength :: Int
sequenceNameLength = 20


-- | The length shown when summarizing sequences.
sequenceSummaryLength :: Int
sequenceSummaryLength = 70

