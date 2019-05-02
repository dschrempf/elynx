{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  EvoMod.Data.Sequence
Description :  Hereditary sequences.
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
  , seqName
  , seqCode
  , seqCharacters
  -- * Input
  -- , toSequence
  -- * Output
  , fromSequence
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
  ) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.List                     (maximumBy)
import           Data.Ord                      (comparing)
-- import qualified Data.Vector.Storable          as V
-- import           Data.Word8                    (Word8)

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.Defaults
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Equality

-- | For now, 'SequenceName's are just 'L.ByteString's.
type SequenceName = L.ByteString

-- | The vector of characters of a sequence.
type SequenceCharacters = L.ByteString

-- | Sequences have a name, a code and hopefully a lot of data.
data Sequence = Sequence { _name       :: SequenceName
                         , _code       :: Code
                         , _characters :: SequenceCharacters }
  deriving (Eq)

makeLenses ''Sequence

-- | Access name.
seqName :: Lens' Sequence SequenceName
seqName = name

-- | Access code.
seqCode :: Lens' Sequence Code
seqCode = code

-- | Access characters.
seqCharacters :: Lens' Sequence SequenceCharacters
seqCharacters = characters

-- | Extract 'SequenceName' and data.
fromSequence :: Sequence -> (SequenceName, L.ByteString)
fromSequence s = (s ^. name, s ^. characters)

showCharacters :: Sequence -> L.ByteString
showCharacters = view characters

showInfo :: Sequence -> L.ByteString
showInfo s = L.unwords [ alignLeft defSequenceNameWidth (s ^. name)
                       , alignRight defFieldWidth (L.pack . show $ s ^. code)
                       , alignLeft defFieldWidth l ]
  where l = L.pack . show $ lengthSequence s

instance Show Sequence where
  show s = L.unpack $ showSequence s

-- | Show a 'Sequence', untrimmed.
showSequence :: Sequence -> L.ByteString
showSequence s = L.unwords [showInfo s, showCharacters s]

-- | Show a list of 'Sequence's, untrimmed.
showSequenceList :: [Sequence] -> L.ByteString
showSequenceList = L.unlines . map showSequence

-- | Header printed before 'Sequence' list.
sequenceListHeader :: L.ByteString
sequenceListHeader = L.unwords [ alignLeft defSequenceNameWidth (L.pack "Name")
                               , alignRight defFieldWidth (L.pack "Code")
                               , alignLeft defFieldWidth (L.pack "Length")
                               , L.pack "Sequence" ]

-- | Trim and show a 'Sequence'.
summarizeSequence :: Sequence -> L.ByteString
summarizeSequence s = L.unwords [ showInfo s
                                , summarizeByteString defSequenceSummaryLength (showCharacters s) ]

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
summarizeSequenceListBody ss = L.unlines $ map summarizeSequence ss

-- | Calculate length of 'Sequence'.
lengthSequence :: Sequence -> Int
lengthSequence s = fromIntegral $ L.length $ s ^. characters

-- | Check if all 'Sequence's have equal length.
equalLength :: [Sequence] -> Bool
equalLength = allEqual . map lengthSequence

-- | Find the longest 'Sequence' in a list.
longest :: [Sequence] -> Sequence
longest = maximumBy (comparing lengthSequence)

-- | Trim to given length.
trimSequence :: Int -> Sequence -> Sequence
trimSequence n = over characters (L.take $ fromIntegral n)

-- | Concatenate two sequences. 'SequenceName's have to match.
concatenate :: Sequence -> Sequence -> Either L.ByteString Sequence
concatenate (Sequence i c cs) (Sequence j k ks)
  | i == j && c == k = Right $ Sequence i c (cs <> ks)
  | otherwise        = Left $ L.pack "concatenate: Sequences do not have equal names: "
                       <> i <> L.pack ", " <> j <> L.pack "."

-- | Concatenate a list of sequences, see 'concatenate'.
concatenateSeqs :: [[Sequence]] -> Either L.ByteString [Sequence]
concatenateSeqs []   = Left $ L.pack "Nothing to concatenate."
concatenateSeqs [ss] = Right ss
concatenateSeqs sss  = foldM (zipWithM concatenate) (head sss) (tail sss)
