{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{- |
Module      :  EvoMod.Data.Sequence
Description :  Hereditary sequences
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:54:51 2018.

XXX: Maybe it is better to use a ByteString for sequences. This would probably
reduce memory overhead a lot. MSAs still need to be saved as matrices, because
there are many column operations that would be awfully slow otherwise.

XXX: Do not export record functions, but provide getters and setters (using
lenses). This way, a change in the data structure does not involve further
refactoring.

-}

module EvoMod.Data.Sequence.Sequence
  ( SequenceName
  , SequenceCharacters
  , Sequence (Sequence)
  -- TODO: No documentation for lenses.
  , name
  , alphName
  , characters
  -- * Input
  , toCharacters
  -- * Output
  , fromCharacters
  , showSequence
  , showSequenceList
  , sequenceListHeader
  , summarizeSequence
  , summarizeSequenceList
  , summarizeSequenceListBody
  -- * Analysis
  , lengthSequence
  , equalLength
  , longest
  -- * Manipulation
  , trimSequence
  , concatenate
  , concatenateSeqs
  -- * Filtering
  , filterShorterThan
  , filterLongerThan
  ) where

import           Control.Lens
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8     as L
import           Data.List                      (maximumBy)
import           Data.Ord                       (comparing)
import qualified Data.Vector.Unboxed            as V
import qualified Text.Printf                    as P

import qualified EvoMod.Data.Alphabet.Alphabet  as A
import           EvoMod.Data.Alphabet.Character
import           EvoMod.Data.Sequence.Defaults
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Equality

-- | For now, 'SequenceName's are just 'L.ByteString's.
type SequenceName = L.ByteString

-- | The vector of characters of a sequence.
type SequenceCharacters = V.Vector Character

-- | Sequences have a name, a code and hopefully a lot of data.
data Sequence = Sequence { _name       :: SequenceName
                         , _alphName   :: A.AlphabetName
                         , _characters :: SequenceCharacters }
  deriving (Eq)

makeLenses ''Sequence

-- | Convert byte string to sequence characters.
toCharacters :: L.ByteString -> SequenceCharacters
toCharacters = V.fromList . map fromChar . L.unpack

-- | Convert sequence characters to byte string.
fromCharacters :: SequenceCharacters -> L.ByteString
fromCharacters = L.pack . map toChar . V.toList

showInfo :: Sequence -> L.ByteString
showInfo s = L.unwords [ alignLeft defSequenceNameWidth (s^.name)
                       , alignRight defFieldWidth (L.pack $ show $ s^.alphName)
                       , alignRight defFieldWidth (L.pack . show $ len)
                       , alignRight defFieldWidth (L.pack $ P.printf "%.3f" pGaps) ]
  where len = lengthSequence s
        nGaps = countGaps s
        pGaps = fromIntegral nGaps / fromIntegral len :: Double

instance Show Sequence where
  show s = L.unpack $ showSequence s

-- | Show a 'Sequence', untrimmed.
showSequence :: Sequence -> L.ByteString
showSequence s = L.unwords [showInfo s, fromCharacters $ s^.characters]

-- | Show a list of 'Sequence's, untrimmed.
showSequenceList :: [Sequence] -> L.ByteString
showSequenceList = L.unlines . map showSequence

-- | Header printed before 'Sequence' list.
sequenceListHeader :: L.ByteString
sequenceListHeader = L.unwords [ alignLeft defSequenceNameWidth (L.pack "Name")
                               , alignRight defFieldWidth (L.pack "Code")
                               , alignRight defFieldWidth (L.pack "Length")
                               , alignRight defFieldWidth (L.pack "Gaps [%]")
                               , L.pack "Sequence" ]

-- | Trim and show a 'Sequence'.
summarizeSequence :: Sequence -> L.ByteString
summarizeSequence s = L.unwords [ showInfo s
                                , summarizeByteString defSequenceSummaryLength
                                  (fromCharacters $ s^.characters) ]

-- | Trim and show a list of 'Sequence's.
summarizeSequenceList :: [Sequence] -> L.ByteString
summarizeSequenceList ss = summarizeSequenceListHeader ss <>
                           summarizeSequenceListBody (take defSequenceListSummaryNumber ss)

summarizeSequenceListHeader :: [Sequence] -> L.ByteString
summarizeSequenceListHeader ss = L.unlines $
  reportIfSubsetIsShown ++
  [ L.pack $ "For each sequence, the " ++ show defSequenceSummaryLength ++ " first bases are shown."
  , L.pack $ "List contains " ++ show (length ss) ++ " sequences."
  , L.pack ""
  , sequenceListHeader ]
  where l = length ss
        s = show defSequenceListSummaryNumber ++ " out of " ++
            show (length ss) ++ " sequences are shown."
        reportIfSubsetIsShown
          | l > defSequenceListSummaryNumber = [L.pack s]
          | otherwise = []

-- | Trim and show a list of 'Sequence's.
summarizeSequenceListBody :: [Sequence] -> L.ByteString
summarizeSequenceListBody ss = L.unlines (map summarizeSequence ss `using` parListChunk 5 rdeepseq)

-- | Calculate length of 'Sequence'.
lengthSequence :: Sequence -> Int
lengthSequence s = fromIntegral $ V.length $ s ^. characters

-- | Check if all 'Sequence's have equal length.
equalLength :: [Sequence] -> Bool
equalLength = allEqual . map lengthSequence

-- | Find the longest 'Sequence' in a list.
longest :: [Sequence] -> Sequence
longest = maximumBy (comparing lengthSequence)

-- | Count number of gaps or unknown characters in sequence.
countGaps :: Sequence -> Int
countGaps s = V.length . V.filter (A.isGap (s^.alphName)) $ s^.characters

-- | Trim to given length.
trimSequence :: Int -> Sequence -> Sequence
trimSequence n = over characters (V.take $ fromIntegral n)

-- | Concatenate two sequences. 'SequenceName's have to match.
concatenate :: Sequence -> Sequence -> Sequence
concatenate (Sequence i c cs) (Sequence j k ks)
  | i == j && c == k = Sequence i c (cs <> ks)
  | otherwise        = error $ "concatenate: Sequences do not have equal names: "
                       ++ L.unpack i ++ ", " ++ L.unpack j ++ "."

-- | Concatenate a list of sequences, see 'concatenate'.
concatenateSeqs :: [[Sequence]] -> [Sequence]
concatenateSeqs []   = error "concatenateSeqs: Nothing to concatenate."
concatenateSeqs [ss] = ss
concatenateSeqs sss  = foldl1 (zipWith concatenate) sss

-- | Only take 'Sequence's that are shorter than a given number.
filterShorterThan :: Int -> [Sequence] -> [Sequence]
filterShorterThan n = filter (\x -> lengthSequence x < n)

-- | Only take 'Sequence's that are longer than a given number.
filterLongerThan :: Int -> [Sequence] -> [Sequence]
filterLongerThan n = filter (\x -> lengthSequence x > n)
