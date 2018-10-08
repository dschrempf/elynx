{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , parseSequence
  , summarizeSequence
  , lengthSequence
  , equalLength
  , longest
  , filterLongerThan
  , NamedSequence (..)
  , showSequenceName
  , summarizeNamedSequence
  , summarizeNamedSequenceList
  , summarizeNamedSequenceListHeader
  , summarizeNamedSequenceListBody
  , lengthNamedSequence
  , equalLengthNamedSequence
  , longestNamedSequence
  , filterLongerThanNamedSequence
  ) where

import           Data.List          (maximumBy)
import           Data.Ord           (comparing)
import           Text.Megaparsec

import           Evol.Data.Alphabet
import           Evol.Data.Defaults (Parser, sequenceNameLength,
                                     sequenceSummaryLength)
import           Evol.Tools         (alignLeft, allEqual)

-- Do I even need an unnamed sequence?
newtype Sequence a = Sequence { fromSequence :: [a] }
  deriving (Read, Eq, Semigroup, Monoid, Foldable)

instance Show a => Show (Sequence a) where
  show (Sequence xs) = concatMap show xs

parseSequence :: Alphabet a => Parser (Sequence a)
parseSequence = Sequence <$> some parseChar

summarizeSequence :: Show a => Sequence a -> String
summarizeSequence s = if length s <= sequenceSummaryLength
                      then concatMap show . fromSequence $ s
                      else (concatMap show . take sequenceSummaryLength . fromSequence) s ++ "..."

lengthSequence :: Sequence a -> Int
lengthSequence = length

equalLength :: [Sequence a] -> Bool
equalLength = allEqual . map lengthSequence

longest :: [Sequence a] -> Sequence a
longest = maximumBy (comparing lengthSequence)

filterLongerThan :: Int -> [Sequence a] -> [Sequence a]
filterLongerThan n = filter (\x -> lengthSequence x > n)

data NamedSequence a = NamedSequence { name :: String
                                     , sequ :: Sequence a }
  deriving (Eq)

showSequenceName :: String -> String
showSequenceName = alignLeft sequenceNameLength

instance Show a => Show (NamedSequence a) where
  show (NamedSequence n s) = showSequenceName n ++ show s

summarizeNamedSequence :: Show a => NamedSequence a -> String
summarizeNamedSequence NamedSequence{sequ=s, name=n} =
  showSequenceName n ++ summarizeSequence s

summarizeNamedSequenceList :: (Show a, Alphabet a) => [NamedSequence a] -> String
summarizeNamedSequenceList ns = summarizeNamedSequenceListHeader "List" ns ++ "\n" ++ summarizeNamedSequenceListBody ns

summarizeNamedSequenceListHeader :: (Show a, Alphabet a) => String -> [NamedSequence a] -> String
summarizeNamedSequenceListHeader h ns = unlines
  [ h ++ " contains " ++ show (length ns) ++ " sequences."
  , "Alphabet: " ++ show a ++ "."
  , "Showing first " ++ show sequenceSummaryLength ++ " bases." ]
  where a = alphabetName . head . fromSequence . sequ . head $ ns

summarizeNamedSequenceListBody :: Show a => [NamedSequence a] -> String
summarizeNamedSequenceListBody ns = unlines $ map summarizeNamedSequence ns

lengthNamedSequence :: NamedSequence a -> Int
lengthNamedSequence = lengthSequence . sequ

equalLengthNamedSequence :: [NamedSequence a] -> Bool
equalLengthNamedSequence = equalLength . map sequ

longestNamedSequence :: [NamedSequence a] -> NamedSequence a
longestNamedSequence = maximumBy (comparing $ lengthSequence . sequ)

filterLongerThanNamedSequence :: Int -> [NamedSequence a] -> [NamedSequence a]
filterLongerThanNamedSequence n = filter (\x -> lengthNamedSequence x > n)
