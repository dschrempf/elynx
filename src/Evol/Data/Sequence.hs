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

import           Evol.Data.Alphabet
import           Evol.Defaults      (defSequenceNameLength,
                                     defSequenceSummaryLength)
import           Evol.Tools         (alignLeft, allEqual)

data Sequence i a = Sequence { seqId :: i
                             , seqCs :: [a] }
  deriving (Read, Eq)

rmFirstQuote :: String -> String
rmFirstQuote ('\"':xs) = xs
rmFirstQuote xs        = xs

rmLastQuote :: String -> String
rmLastQuote = reverse . rmFirstQuote . reverse

rmDoubleQuotes :: String -> String
rmDoubleQuotes = rmFirstQuote . rmLastQuote


showSequenceId :: Show i => i -> String
showSequenceId = alignLeft defSequenceNameLength . show'
-- XXX: Remove double quotes in case i is of type 'String'.
  where show' = rmDoubleQuotes . show

instance (Show i, Show a) => Show (Sequence i a) where
  show (Sequence i cs) = showSequenceId i ++ concatMap show cs

summarizeCharacters :: Show a => [a] -> String
summarizeCharacters cs = if length cs <= defSequenceSummaryLength
                         then concatMap show cs
                         else (concatMap show . take defSequenceSummaryLength) cs ++ "..."

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
summarizeSequenceList ss = summarizeSequenceListHeader "List" ss ++ summarizeSequenceListBody ss

summarizeSequenceListHeader :: (Show a, Alphabet a) => String -> [Sequence i a] -> String
summarizeSequenceListHeader h ss = unlines
  [ h ++ " contains " ++ show (length ss) ++ " sequences."
  , "Alphabet: " ++ show a ++ "."
  , "Showing first " ++ show defSequenceSummaryLength ++ " bases."
  , ""
  , showSequenceId "Identifier" ++ "Sequence" ]
  where a = alphabetName . head . seqCs . head $ ss

summarizeSequenceListBody :: (Show i, Show a) => [Sequence i a] -> String
summarizeSequenceListBody ss = unlines $ map summarizeSequence ss
