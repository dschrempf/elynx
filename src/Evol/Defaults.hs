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
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Void
import           Text.Megaparsec

type Parser = Parsec Void ByteString

-- | Space reserved for sequence names when printing them.
defSequenceNameLength :: Int
defSequenceNameLength = 20


-- | The length shown when summarizing sequences.
defSequenceSummaryLength :: Int
defSequenceSummaryLength = 60

