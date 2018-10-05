{- |
Module      :  MultiSequenceAlignment
Description :  Multi sequence alignment related types and functions.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:40:18 2018.

-}


module Base.MultiSequenceAlignment
  ( MultiSequenceAlignment (..)
  , showSummaryMSA
  , join
  , equalNrSites
  , lengthMSA
  ) where

import           Base.Alphabet
import           Base.Sequence
import           Tools         (allEqual)

data MultiSequenceAlignment a = MSA { sequences  :: [NamedSequence a]
                                    , nSequences :: Int }

instance Show a => Show (MultiSequenceAlignment a) where
  show MSA{sequences=xs} = unlines $ (showSequenceName "Name" ++ "Sequence") : map show xs

showSummaryMSA :: (Show a, Alphabet a) => MultiSequenceAlignment a -> String
showSummaryMSA MSA{sequences=xs, nSequences=n} = unlines $
  [ "Multiple sequence alignment contains " ++ show n ++ " sequences."
  , "Alphabet: " ++ show a ++ "." ]
  ++ map summarizeNamedSequence xs
  where a = alphabetName . head . fromSequence . sequ . head $ xs

join :: MultiSequenceAlignment a -> MultiSequenceAlignment a -> MultiSequenceAlignment a
join (MSA xs nsx) (MSA ys nsy) = MSA (xs ++ ys) (nsx + nsy)

equalNrSites :: MultiSequenceAlignment a -> Bool
equalNrSites MSA{sequences=xs} = allEqual $ map lengthNamedSequence xs

lengthMSA :: MultiSequenceAlignment a -> Maybe Int
lengthMSA msa@MSA{sequences=xs} | equalNrSites msa = Just $ lengthNamedSequence (head xs)
                                | otherwise        = Nothing
