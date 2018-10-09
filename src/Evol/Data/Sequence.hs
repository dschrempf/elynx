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

import           Data.List           (maximumBy)
import           Data.Ord            (comparing)
import qualified Data.Vector.Unboxed as V

import           Evol.Data.Alphabet
import           Evol.Defaults       (defSequenceListSummaryNumber,
                                      defSequenceNameLength,
                                      defSequenceSummaryLength)
import           Evol.Tools          (alignLeft, allEqual)

data Sequence i a = Sequence { seqId :: i
                             , seqCs :: V.Vector a }
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

instance (Show i, Show a, V.Unbox a) => Show (Sequence i a) where
  show (Sequence i cs) = showSequenceId i ++ (concatMap show . V.toList $ cs)

summarizeCharacters :: (Show a, V.Unbox a) => V.Vector a -> String
summarizeCharacters cs = if V.length cs <= defSequenceSummaryLength
                         then concatMap show . V.toList $ cs
                         else (concatMap show . V.toList . V.take defSequenceSummaryLength $ cs)
                              ++ "..."

summarizeSequence :: (Show i, Show a, V.Unbox a) => Sequence i a -> String
summarizeSequence Sequence{seqId=i, seqCs=cs} =
  showSequenceId i ++ summarizeCharacters cs

lengthSequence :: (V.Unbox a) => Sequence i a -> Int
lengthSequence = V.length . seqCs

equalLength :: (V.Unbox a) => [Sequence i a] -> Bool
equalLength = allEqual . map lengthSequence

longest :: (V.Unbox a) => [Sequence i a] -> Sequence i a
longest = maximumBy (comparing lengthSequence)

filterLongerThan :: (V.Unbox a) => Int -> [Sequence i a] -> [Sequence i a]
filterLongerThan n = filter (\x -> lengthSequence x > n)

summarizeSequenceList :: (Show i, Show a, Alphabet a, V.Unbox a) => [Sequence i a] -> String
summarizeSequenceList ss = summarizeSequenceListHeader "List" ss ++ summarizeSequenceListBody (take defSequenceListSummaryNumber ss)

summarizeSequenceListHeader :: (Show a, Alphabet a, V.Unbox a) => String -> [Sequence i a] -> String
summarizeSequenceListHeader h ss = unlines $
  [ h ++ " contains " ++ show (length ss) ++ " sequences."
  , "Alphabet: " ++ show a ++ "."
  , "Showing first " ++ show defSequenceSummaryLength ++ " bases." ]
  ++ reportIfSubsetIsShown ++
  [ ""
  , showSequenceId "Identifier" ++ "Sequence" ]
  where a = alphabetName . V.head . seqCs . head $ ss
        reportIfSubsetIsShown
          | length ss > defSequenceListSummaryNumber =
              [ "Showing " ++ show defSequenceListSummaryNumber ++
              " out of " ++ show (length ss) ++ " sequences."]
          | otherwise = []

summarizeSequenceListBody :: (Show i, Show a, V.Unbox a) => [Sequence i a] -> String
summarizeSequenceListBody ss = unlines $ map summarizeSequence ss
