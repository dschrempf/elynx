{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  Sequence
Description :  Hereditary sequences.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:54:51 2018.

-}


module Base.Sequence
  ( Sequence (..)
  , parseSequence
  , summarizeSequence
  , nSitesSequence
  , equalNumberOfSites
  , mostSites
  , filterMoreSitesThan
  , NamedSequence (..)
  , showSequenceName
  , summarizeNamedSequence
  , nSitesNamedSequence
  , equalNumberOfSitesNamedSequence
  ) where

import           Data.List       (maximumBy)
import           Data.Ord        (comparing)
import           Text.Megaparsec

import           Base.Alphabet
import           Base.Defaults   (Parser, sequenceNameLength,
                                  sequenceSummaryLength)
import           Tools           (alignLeft, allEqual)

newtype Sequence a = Sequence { fromSequence :: [a] }
  deriving (Semigroup, Monoid, Foldable)

instance Show a => Show (Sequence a) where
  show (Sequence xs) = concatMap show xs

parseSequence :: Alphabet a => Parser (Sequence a)
parseSequence = Sequence <$> some parseChar

summarizeSequence :: Show a => Sequence a -> String
summarizeSequence s = (show . take sequenceSummaryLength . fromSequence $ s) ++ "..."

nSitesSequence :: Sequence a -> Int
nSitesSequence = length

equalNumberOfSites :: [Sequence a] -> Bool
equalNumberOfSites = allEqual . map nSitesSequence

-- TODO: Check if this really gets the longest (i.e., if the direction of
-- comparing is as expected); use test suite.
mostSites :: [Sequence a] -> Sequence a
mostSites = maximumBy (comparing nSitesSequence)

-- TODO: Test this in test suite.
filterMoreSitesThan :: Int -> [Sequence a] -> [Sequence a]
filterMoreSitesThan n = filter (\x -> nSitesSequence x > n)

data NamedSequence a = NamedSequence { name :: String
                                     , sequ :: Sequence a }

showSequenceName :: String -> String
showSequenceName = alignLeft sequenceNameLength

instance Show a => Show (NamedSequence a) where
  show (NamedSequence n s) = showSequenceName n ++ show s

summarizeNamedSequence :: Show a => NamedSequence a -> String
summarizeNamedSequence NamedSequence{sequ=s, name=n} =
  showSequenceName n ++ summarizeSequence s

nSitesNamedSequence :: NamedSequence a -> Int
nSitesNamedSequence = nSitesSequence . sequ

equalNumberOfSitesNamedSequence :: [NamedSequence a] -> Bool
equalNumberOfSitesNamedSequence = equalNumberOfSites . map sequ
