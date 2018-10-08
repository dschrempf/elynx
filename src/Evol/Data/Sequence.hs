{- |
Module      :  Evol.Data.Sequence
Description :  Hereditary sequences.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:54:51 2018.

-}


module Evol.Data.Sequence
  ( Sequence (..)
  -- , parseSequence
  , showSequenceId
  , summarizeCharacters
  , summarizeSequence
  , lengthSequence
  , equalLength
  , longest
  , filterLongerThan
  , summarizeSequenceList
  , summarizeSequenceListHeader
  , summarizeSequenceListBody
  ) where

import           Data.List          (maximumBy)
import           Data.Ord           (comparing)
-- import           Text.Megaparsec

import           Evol.Data.Alphabet
import           Evol.Data.Defaults (sequenceNameLength, sequenceSummaryLength)
import           Evol.Tools         (alignLeft, allEqual)

-- Do I even need an unnamed sequence?
data Sequence i a = Sequence { seqId :: i
                             , seqCs :: [a] }
  deriving (Read, Eq)

showSequenceId :: Show i => i -> String
showSequenceId = alignLeft sequenceNameLength . show

instance (Show i, Show a) => Show (Sequence i a) where
  show (Sequence i cs) = showSequenceId i ++ concatMap show cs

summarizeCharacters :: Show a => [a] -> String
summarizeCharacters cs = if length cs <= sequenceSummaryLength
                         then concatMap show cs
                         else (concatMap show . take sequenceSummaryLength) cs ++ "..."

summarizeSequence :: (Show i, Show a) => Sequence i a -> String
summarizeSequence Sequence{seqId=i, seqCs=cs} =
  showSequenceId i ++ summarizeCharacters cs

lengthSequence :: Sequence i a -> Int
lengthSequence = length . seqCs

equalLength :: [Sequence i a] -> Bool
equalLength = allEqual . map lengthSequence

longest :: [Sequence i a] -> Sequence i a
longest = maximumBy (comparing lengthSequence)

filterLongerThan :: Int -> [Sequence i a] -> [Sequence i a]
filterLongerThan n = filter (\x -> lengthSequence x > n)

summarizeSequenceList :: (Show i, Show a, Alphabet a) => [Sequence i a] -> String
summarizeSequenceList ss = summarizeSequenceListHeader "List" ss ++
                           "\n" ++ summarizeSequenceListBody ss

summarizeSequenceListHeader :: (Show a, Alphabet a) => String -> [Sequence i a] -> String
summarizeSequenceListHeader h ss = unlines
  [ h ++ " contains " ++ show (length ss) ++ " sequences."
  , "Alphabet: " ++ show a ++ "."
  , "Showing first " ++ show sequenceSummaryLength ++ " bases." ]
  where a = alphabetName . head . seqCs . head $ ss

summarizeSequenceListBody :: (Show i, Show a) => [Sequence i a] -> String
summarizeSequenceListBody ss = unlines $ map summarizeSequence ss

