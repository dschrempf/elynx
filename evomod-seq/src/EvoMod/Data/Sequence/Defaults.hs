{- |
Module      :  EvoMod.Defaults
Description :  Various default values
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 23:00:17 2018.

-}


module EvoMod.Data.Sequence.Defaults
  ( defSequenceNameWidth
  , defSequenceSummaryLength
  , defSequenceListSummaryNumber
  , defFieldWidth
  ) where

-- | Space reserved for sequence names when printing them.
defSequenceNameWidth :: Int
defSequenceNameWidth = 23

-- | The length shown when summarizing sequences.
defSequenceSummaryLength :: Int
defSequenceSummaryLength = 60

-- | How many sequences are shown in summary.
defSequenceListSummaryNumber :: Int
defSequenceListSummaryNumber = 200

-- | Field width for tables.
defFieldWidth :: Int
defFieldWidth = 13
