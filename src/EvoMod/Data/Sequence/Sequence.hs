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
  , concatenateSeqs
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.List                     (maximumBy)
import           Data.Ord                      (comparing)
import qualified Data.Vector.Unboxed           as V
import           Data.Word8                    (Word8)

import           EvoMod.Data.Sequence.Defaults (defFieldWidth,
                                                defSequenceListSummaryNumber,
                                                defSequenceNameWidth,
                                                defSequenceSummaryLength)
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Equality

-- | For now, 'SequenceId's are just 'String's.
type SequenceId = B.ByteString

-- | By choosing specific types for the identifier and the characters is of
-- course limiting but also eases handling of types a lot.
data Sequence = Sequence { seqId :: SequenceId
                         , seqCs :: V.Vector Word8 }
  deriving (Eq)

-- | Conversion from 'B.ByteString'.
toSequence :: B.ByteString -> B.ByteString -> Sequence
toSequence i cs = Sequence i v
  where v = V.force . V.fromList . map c2w . B.unpack $ cs

-- | Conversion of data to 'B.ByteString'.
seqToCsByteString :: Sequence -> B.ByteString
seqToCsByteString = B.pack . map w2c . V.toList . seqCs

-- | Extract 'SequenceId' and data.
fromSequence :: Sequence -> (SequenceId, B.ByteString)
fromSequence s = (seqId s, seqToCsByteString s)

showCharacters :: Sequence -> B.ByteString
showCharacters = seqToCsByteString

showInfo :: Sequence -> B.ByteString
showInfo s = B.unwords [ alignLeft defSequenceNameWidth (seqId s)
                       , alignLeft defFieldWidth l ]
  where l = B.pack . show $ V.length . seqCs $ s

instance Show Sequence where
  show s = B.unpack $ showSequence s

showSequence :: Sequence -> B.ByteString
showSequence s = B.unwords [showInfo s, showCharacters s]

-- | Show a list of 'Sequence's, untrimmed.
showSequenceList :: [Sequence] -> B.ByteString
showSequenceList = B.unlines . map showSequence

-- | Header printed before 'Sequence' list.
sequenceListHeader :: B.ByteString
sequenceListHeader = B.unwords [ alignLeft defSequenceNameWidth (B.pack "Identifier")
                               , alignLeft defFieldWidth (B.pack "Length")
                               , B.pack "Sequence" ]

-- | Trim and show a 'Sequence'.
summarizeSequence :: Sequence -> B.ByteString
summarizeSequence s = B.unwords [ showInfo s
                                , summarizeByteString defSequenceSummaryLength (showCharacters s) ]

-- | Trim and show a list of 'Sequence's.
summarizeSequenceList :: [Sequence] -> B.ByteString
summarizeSequenceList ss = summarizeSequenceListHeader ss <>
                           summarizeSequenceListBody (take defSequenceListSummaryNumber ss)

summarizeSequenceListHeader :: [Sequence] -> B.ByteString
summarizeSequenceListHeader ss = B.unlines $
  reportIfSubsetIsShown ++
  [ B.pack $ "For each sequence the " ++ show defSequenceSummaryLength ++ " first bases are shown."
  , B.pack $ "List contains " ++ show (length ss) ++ " sequences."
  , B.pack ""
  , sequenceListHeader ]
  where l = length ss
        s = show defSequenceListSummaryNumber ++ " out of " ++
            show (length ss) ++ " sequences are shown."
        reportIfSubsetIsShown
          | l > defSequenceListSummaryNumber = [B.pack s]
          | otherwise = []

-- | Trim and show a list of 'Sequence's.
summarizeSequenceListBody :: [Sequence] -> B.ByteString
summarizeSequenceListBody ss = B.unlines $ map summarizeSequence ss

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
concatenate :: Sequence -> Sequence -> Either B.ByteString Sequence
concatenate (Sequence i cs) (Sequence j ks)
  | i == j     = Right $ Sequence i (cs V.++ ks)
  | otherwise  = Left $ B.pack "concatenate: Sequences do not have equal IDs: "
                 <> i <> B.pack ", " <> j <> B.pack "."

-- | Concatenate a list of sequences, see 'concatenate'.
concatenateSeqs :: [[Sequence]] -> Either B.ByteString [Sequence]
concatenateSeqs []   = Left $ B.pack "Nothing to concatenate."
concatenateSeqs [ss] = Right ss
concatenateSeqs sss  = foldM (zipWithM concatenate) (head sss) (tail sss)
