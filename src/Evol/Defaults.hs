{- |
Module      :  Evol.Defaults
Description :  Various default values.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 23:00:17 2018.

-}


module Evol.Defaults
  ( Parser
  , defSequenceNameLength
  , defSequenceSummaryLength
  , defSequenceListSummaryNumber
  ) where

import qualified Data.ByteString.Lazy as B
import           Data.Void
import           Text.Megaparsec

type Parser = Parsec Void B.ByteString

-- | Space reserved for sequence names when printing them.
defSequenceNameLength :: Int
defSequenceNameLength = 25


-- | The length shown when summarizing sequences.
defSequenceSummaryLength :: Int
defSequenceSummaryLength = 60

-- | How many sequences are shown in summary.
defSequenceListSummaryNumber :: Int
defSequenceListSummaryNumber = 100
