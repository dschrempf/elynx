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
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.List                     (maximumBy)
import           Data.Ord                      (comparing)
import qualified Data.Vector.Storable          as V
import           Data.Word8                    (Word8)

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.Defaults (defFieldWidth,
                                                defSequenceListSummaryNumber,
                                                defSequenceNameWidth,
                                                defSequenceSummaryLength)
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Equality

-- | For now, 'SequenceId's are just 'String's.
type SequenceId = L.ByteString

-- | By choosing specific types for the identifier and the characters is of
-- course limiting but also eases handling of types a lot.
data Sequence = Sequence { seqId   :: SequenceId
                         , seqCode :: Code
                         , seqCs   :: V.Vector Word8 }
  deriving (Eq)

-- | Conversion from 'L.ByteString'.
toSequence :: L.ByteString -> Code -> L.ByteString -> Sequence
toSequence i c cs = Sequence i c v
  where v = V.fromList . map c2w . L.unpack $ cs

-- | Conversion of data to 'L.ByteString'.
seqToCsByteString :: Sequence -> L.ByteString
seqToCsByteString = L.pack . map w2c . V.toList . seqCs

-- | Extract 'SequenceId' and data.
fromSequence :: Sequence -> (SequenceId, L.ByteString)
fromSequence s = (seqId s, seqToCsByteString s)

showCharacters :: Sequence -> L.ByteString
showCharacters = seqToCsByteString

showInfo :: Sequence -> L.ByteString
showInfo s = L.unwords [ alignLeft defSequenceNameWidth (seqId s)
                       , alignLeft defFieldWidth l ]
  where l = L.pack . show $ lengthSequence s

instance Show Sequence where
  show s = L.unpack $ showSequence s

showSequence :: Sequence -> L.ByteString
showSequence s = L.unwords [showInfo s, showCharacters s]

-- | Show a list of 'Sequence's, untrimmed.
showSequenceList :: [Sequence] -> L.ByteString
showSequenceList = L.unlines . map showSequence

-- | Header printed before 'Sequence' list.
sequenceListHeader :: L.ByteString
sequenceListHeader = L.unwords [ alignLeft defSequenceNameWidth (L.pack "Identifier")
                               , alignLeft defFieldWidth (L.pack "Length")
                               , L.pack "Sequence" ]

-- | Trim and show a 'Sequence'.
summarizeSequence :: Sequence -> L.ByteString
summarizeSequence s = L.unwords [ showInfo s
                                , summarizeByteString defSequenceSummaryLength (showCharacters s) ]

-- TODO: Print codes! Then, MSAs have to be printed differently, because code
-- has to be the same in this case.
-- | Trim and show a list of 'Sequence's.
summarizeSequenceList :: [Sequence] -> L.ByteString
summarizeSequenceList ss = summarizeSequenceListHeader ss <>
                           summarizeSequenceListBody (take defSequenceListSummaryNumber ss)

summarizeSequenceListHeader :: [Sequence] -> L.ByteString
summarizeSequenceListHeader ss = L.unlines $
  reportIfSubsetIsShown ++
  [ L.pack $ "For each sequence the " ++ show defSequenceSummaryLength ++ " first bases are shown."
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
summarizeSequenceListBody ss = L.unlines $ map summarizeSequence ss

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
concatenate :: Sequence -> Sequence -> Either L.ByteString Sequence
concatenate (Sequence i code cs) (Sequence j kode ks)
  | i == j && code == kode = Right $ Sequence i code (cs V.++ ks)
  | otherwise              = Left $ L.pack "concatenate: Sequences do not have equal IDs: "
                             <> i <> L.pack ", " <> j <> L.pack "."

-- | Concatenate a list of sequences, see 'concatenate'.
concatenateSeqs :: [[Sequence]] -> Either L.ByteString [Sequence]
concatenateSeqs []   = Left $ L.pack "Nothing to concatenate."
concatenateSeqs [ss] = Right ss
concatenateSeqs sss  = foldM (zipWithM concatenate) (head sss) (tail sss)
