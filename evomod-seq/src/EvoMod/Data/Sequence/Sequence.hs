{-# LANGUAGE TemplateHaskell #-}

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
  , seqName
  , seqCode
  , seqCharacters
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
  , translate
  -- * Filtering
  , filterShorterThan
  , filterLongerThan
  ) where

import           Control.Lens
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8     as L
import           Data.List                      (maximumBy)
import qualified Data.Map                       as M
import           Data.Ord                       (comparing)
import qualified Data.Vector.Unboxed            as V
import           Text.Printf

import           EvoMod.Data.Alphabet.Alphabet
import qualified EvoMod.Data.Alphabet.Character as C
import           EvoMod.Data.Alphabet.Codon
import           EvoMod.Data.Sequence.Defaults
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.Equality
import           EvoMod.Tools.List

-- | For now, 'SequenceName's are just 'L.ByteString's.
type SequenceName = L.ByteString

-- | The vector of characters of a sequence.
type SequenceCharacters = V.Vector C.Character

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

-- | Converrt byte string to sequence characters.
toCharacters :: L.ByteString -> SequenceCharacters
toCharacters = V.fromList . map C.fromChar . L.unpack

-- | Convert sequence characters to byte string.
fromCharacters :: SequenceCharacters -> L.ByteString
-- Seriously?
fromCharacters = L.pack . map C.toChar . V.toList

showInfo :: Sequence -> L.ByteString
showInfo s = L.unwords [ alignLeft defSequenceNameWidth (s^.name)
                       , alignRight defFieldWidth (L.pack . show $ s^.code)
                       , alignRight defFieldWidth (L.pack . show $ len)
                       , alignRight defFieldWidth (L.pack $ printf "%.3f" pGaps) ]
  where len = lengthSequence s
        nGaps = countGapOrUnknownChars s
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

-- XXX This is pretty hacky here. Better to change to vector (not byte string).
-- | Count number of gaps or unknown characters in sequence.
countGapOrUnknownChars :: Sequence -> Int
countGapOrUnknownChars s = V.length . V.filter (isGapOrUnknown cd) $ s^.characters
  where cd = s^.code

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

-- TODO: This function goes via lists. Super slow.
-- | Translate from DNA to Protein with given reading frame (0, 1, 2).
translate :: Int -> Sequence -> Sequence
translate rf (Sequence n c cs) | rf > 2    = error "translate: reading frame is larger than 2."
                               | rf < 0    = error "translate: reading frame is negative."
                               | c == DNA  = Sequence n Protein aas
                               | c == DNAX = Sequence n ProteinX aas
                               | otherwise = error "translate: can only translate DNA to Protein."
  where codons = map Codon $ chop3 $ V.toList $ V.drop rf cs
        aas = V.fromList $ map (universalCode M.!) codons

-- | Only take 'Sequence's that are shorter than a given number.
filterShorterThan :: Int -> [Sequence] -> [Sequence]
filterShorterThan n = filter (\x -> lengthSequence x < n)

-- | Only take 'Sequence's that are longer than a given number.
filterLongerThan :: Int -> [Sequence] -> [Sequence]
filterLongerThan n = filter (\x -> lengthSequence x > n)
