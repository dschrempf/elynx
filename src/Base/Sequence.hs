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
  , NamedSequence (..)
  , showSequenceName
  , summarizeNamedSequence
  , lengthNamedSequence
  ) where

import           Data.Attoparsec.Text (Parser, many1)

import           Base.Alphabet
import           Base.Defaults        (sequenceNameLength,
                                       sequenceSummaryLength)
import           Tools                (alignLeft)

newtype Sequence a = Sequence { fromSequence :: [a] }
  deriving (Semigroup, Monoid, Foldable)

instance Show a => Show (Sequence a) where
  show (Sequence xs) = concatMap show xs

parseSequence :: Alphabet a => Parser (Sequence a)
parseSequence = Sequence <$> many1 parseChar

summarizeSequence :: Show a => Sequence a -> String
summarizeSequence s = (show . take sequenceSummaryLength . fromSequence $ s) ++ "..."

data NamedSequence a = NamedSequence { name :: String
                                     , sequ :: Sequence a }

showSequenceName :: String -> String
showSequenceName = alignLeft sequenceNameLength

instance Show a => Show (NamedSequence a) where
  show (NamedSequence n s) = showSequenceName n ++ show s

summarizeNamedSequence :: Show a => NamedSequence a -> String
summarizeNamedSequence NamedSequence{sequ=s, name=n} = showSequenceName n ++ summarizeSequence p
  where p = Sequence $ take sequenceSummaryLength $ fromSequence s

lengthNamedSequence :: NamedSequence a -> Int
lengthNamedSequence NamedSequence {sequ=x} = length x
