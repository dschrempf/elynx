{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  ELynx.Data.Sequence
Description :  Hereditary sequences
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:54:51 2018.

This module is to be imported qualified.

-}

module ELynx.Data.Sequence.Sequence
  ( -- * Types
    Name
  , Characters
  , Sequence (Sequence)
  -- * Lenses
  , name
  , alphabet
  , characters
  -- * Input
  , toCharacters
  -- * Output
  , fromCharacters
  , toByteString
  , header
  , summarize
  , summarizeSequences
  , body
  -- * Analysis
  , length
  , equalLength
  , longest
  -- * Manipulation
  , trim
  , concat
  , concatSequences
  -- * Filtering
  , filterShorterThan
  , filterLongerThan
  ) where

import           Control.Lens
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.List                     (maximumBy)
import           Data.Ord                      (comparing)
import qualified Data.Vector.Unboxed           as V
import           Prelude                       hiding (concat, length)
import qualified Prelude                       as Pr (length)
import qualified Text.Printf                   as P

import qualified ELynx.Data.Alphabet.Alphabet  as A
import           ELynx.Data.Alphabet.Character
import           ELynx.Data.Sequence.Defaults
import           ELynx.Tools.ByteString
import           ELynx.Tools.Equality

-- | For now, 'Name's are just 'L.ByteString's.
type Name = L.ByteString

-- | The vector of characters of a sequence.
type Characters = V.Vector Character

-- | Sequences have a name, a code and hopefully a lot of data.
data Sequence = Sequence { _name       :: Name
                         , _alphabet   :: A.Alphabet
                         , _characters :: Characters }
  deriving (Eq)

makeLenses ''Sequence

-- | Convert byte string to sequence characters.
toCharacters :: L.ByteString -> Characters
toCharacters = V.fromList . map fromChar . L.unpack

-- | Convert sequence characters to byte string.
fromCharacters :: Characters -> L.ByteString
fromCharacters = L.pack . map toChar . V.toList

showInfo :: Sequence -> L.ByteString
showInfo s = L.unwords [ alignLeft nameWidth (s^.name)
                       , alignRight fieldWidth (L.pack $ show $ s^.alphabet)
                       , alignRight fieldWidth (L.pack . show $ len)
                       , alignRight fieldWidth (L.pack $ P.printf "%.3f" pGaps) ]
  where len = length s
        nGaps = countGaps s
        pGaps = fromIntegral nGaps / fromIntegral len :: Double

instance Show Sequence where
  show s = L.unpack $ toByteString s

-- | Show a 'Sequence', untrimmed.
toByteString :: Sequence -> L.ByteString
toByteString s = L.unwords [showInfo s, fromCharacters $ s^.characters]

-- | Trim and show a 'Sequence'.
summarize :: Sequence -> L.ByteString
summarize s = L.unwords [ showInfo s
                        , summarizeByteString summaryLength
                          (fromCharacters $ s^.characters) ]

-- | Trim and show a list of 'Sequence's.
summarizeSequences :: [Sequence] -> L.ByteString
summarizeSequences ss = header ss <>
                 body (take summaryNSequences ss)

-- | Header printed before 'Sequence' list.
tableHeader :: L.ByteString
tableHeader = L.unwords [ alignLeft  nameWidth "Name"
                        , alignRight fieldWidth        "Code"
                        , alignRight fieldWidth        "Length"
                        , alignRight fieldWidth        "Gaps [%]"
                        , "Sequence" ]

header :: [Sequence] -> L.ByteString
header ss = L.unlines $
  reportIfSubsetIsShown ++
  [ L.pack $ "For each sequence, the " ++ show summaryLength ++ " first bases are shown."
  , L.pack $ "List contains " ++ show (Pr.length ss) ++ " sequences."
  , ""
  , tableHeader ]
  where l = Pr.length ss
        s = show summaryNSequences ++ " out of " ++
            show (Pr.length ss) ++ " sequences are shown."
        reportIfSubsetIsShown
          | l > summaryNSequences = [L.pack s]
          | otherwise = []

-- | Trim and show a list of 'Sequence's.
body :: [Sequence] -> L.ByteString
body ss = L.unlines (map summarize ss `using` parListChunk 5 rdeepseq)

-- | Calculate length of 'Sequence'.
length :: Sequence -> Int
length s = fromIntegral $ V.length $ s ^. characters

-- | Check if all 'Sequence's have equal length.
equalLength :: [Sequence] -> Bool
equalLength = allEqual . map length

-- | Find the longest 'Sequence' in a list.
longest :: [Sequence] -> Sequence
longest = maximumBy (comparing length)

-- | Count number of gaps or unknown characters in sequence.
countGaps :: Sequence -> Int
countGaps s = V.length . V.filter (A.isGap (s^.alphabet)) $ s^.characters

-- | Trim to given length.
trim :: Int -> Sequence -> Sequence
trim n = over characters (V.take $ fromIntegral n)

-- | Concatenate two sequences. 'Name's have to match.
concat :: Sequence -> Sequence -> Sequence
concat (Sequence i c cs) (Sequence j k ks)
  | i == j && c == k = Sequence i c (cs <> ks)
  | otherwise        = error $ "concatenate: Sequences do not have equal names: "
                       ++ L.unpack i ++ ", " ++ L.unpack j ++ "."

-- | Concatenate a list of sequences, see 'concat'.
concatSequences :: [[Sequence]] -> [Sequence]
concatSequences []   = error "concatenateSequences: Nothing to concatenate."
concatSequences [ss] = ss
concatSequences sss  = foldl1 (zipWith concat) sss

-- | Only take 'Sequence's that are shorter than a given number.
filterShorterThan :: Int -> [Sequence] -> [Sequence]
filterShorterThan n = filter (\x -> length x < n)

-- | Only take 'Sequence's that are longer than a given number.
filterLongerThan :: Int -> [Sequence] -> [Sequence]
filterLongerThan n = filter (\x -> length x > n)
