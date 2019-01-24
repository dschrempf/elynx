{- |
Module      :  EvoMod.Data.Sequence
Description :  Hereditary sequences.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:54:51 2018.

-}

module EvoMod.Data.Sequence.Sequence
  ( SequenceId
  , Sequence(..)
  -- * Input
  , toSequence
  -- * Output
  , fromSequence
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
  ) where

import qualified Data.ByteString.Lazy          as B
import           Data.List                     (maximumBy)
import           Data.Ord                      (comparing)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Vector.Unboxed           as V
import           Data.Word8                    (Word8)

import           EvoMod.Data.Sequence.Defaults (defFieldWidth,
                                                defSequenceListSummaryNumber,
                                                defSequenceNameWidth,
                                                defSequenceSummaryLength)
import           EvoMod.Tools                  (allEqual,
                                                summarizeText)

-- | For now, 'SequenceId's are just 'String's.
type SequenceId = T.Text

-- | By choosing specific types for the identifier and the characters is of
-- course limiting but also eases handling of types a lot.
data Sequence = Sequence { seqId :: SequenceId
                         , seqCs :: V.Vector Word8 }
  deriving (Eq)

-- | Conversion from 'B.ByteString'.
toSequence :: T.Text -> B.ByteString -> Sequence
toSequence i cs = Sequence i v
  where v = V.force . V.fromList . B.unpack $ cs

-- | Conversion of data to 'B.ByteString'.
seqToCsByteString :: Sequence -> B.ByteString
seqToCsByteString = B.pack . V.toList . seqCs

-- | Extract 'SequenceId' and data.
fromSequence :: Sequence -> (SequenceId, B.ByteString)
fromSequence s = (seqId s, seqToCsByteString s)

showCharacters :: Sequence -> T.Text
showCharacters = T.decodeUtf8 . B.toStrict . seqToCsByteString

showInfo :: Sequence -> T.Text
showInfo s = T.unwords [ T.justifyLeft defSequenceNameWidth ' ' (seqId s)
                       , T.justifyLeft defFieldWidth ' ' l ]
  where l = T.pack . show $ V.length . seqCs $ s

-- instance Show Sequence where
--   show s = showInfo s ++ " " ++ showCharacters s

showSequence :: Sequence -> T.Text
showSequence s = T.unwords [showInfo s, showCharacters s]

-- | Show a list of 'Sequence's, untrimmed.
showSequenceList :: [Sequence] -> T.Text
showSequenceList = T.unlines . map showSequence

-- | Header printed before 'Sequence' list.
sequenceListHeader :: T.Text
sequenceListHeader = T.unwords [ T.justifyLeft defSequenceNameWidth ' ' (T.pack "Identifier")
                               , T.justifyLeft defFieldWidth ' ' (T.pack "Length")
                               , T.pack "Sequence" ]

-- | Trim and show a 'Sequence'.
summarizeSequence :: Sequence -> T.Text
summarizeSequence s = T.unwords [ showInfo s
                                , summarizeText defSequenceSummaryLength (showCharacters s) ]

-- | Trim and show a list of 'Sequence's.
summarizeSequenceList :: [Sequence] -> T.Text
summarizeSequenceList ss = summarizeSequenceListHeader ss <>
                           summarizeSequenceListBody (take defSequenceListSummaryNumber ss)

summarizeSequenceListHeader :: [Sequence] -> T.Text
summarizeSequenceListHeader ss = T.unlines $
  reportIfSubsetIsShown ++
  [ T.pack $ "For each sequence the " ++ show defSequenceSummaryLength ++ " first bases are shown."
  , T.pack $ "List contains " ++ show (length ss) ++ " sequences."
  , T.pack ""
  , sequenceListHeader ]
  where l = length ss
        s = show defSequenceListSummaryNumber ++ " out of " ++
            show (length ss) ++ " sequences are shown."
        reportIfSubsetIsShown
          | l > defSequenceListSummaryNumber = [T.pack s]
          | otherwise = []

-- | Trim and show a list of 'Sequence's.
summarizeSequenceListBody :: [Sequence] -> T.Text
summarizeSequenceListBody ss = T.unlines $ map summarizeSequence ss

-- | Calculate length of 'Sequence'.
lengthSequence :: Sequence -> Int
lengthSequence = V.length . seqCs

-- | Check if all 'Sequence's have equal length.
equalLength :: [Sequence] -> Bool
equalLength = allEqual . map lengthSequence

-- | Find the longest 'Sequence' in a list.
longest :: [Sequence] -> Sequence
longest = maximumBy (comparing lengthSequence)

-- | Trim to given length.
trimSequence :: Int -> Sequence -> Sequence
trimSequence n s@Sequence{seqCs=cs} = s {seqCs = V.take n cs}

-- | Concatenate two sequences. 'SequenceId's have to match.
concatenate :: Sequence -> Sequence -> Either String Sequence
concatenate (Sequence i cs) (Sequence j ks)
  | i == j     = Right $ Sequence i (cs V.++ ks)
  | otherwise  = Left $ "concatenate: Sequences do not have equal IDs: "
                 ++ T.unpack i ++ ", " ++ T.unpack j ++ "."
