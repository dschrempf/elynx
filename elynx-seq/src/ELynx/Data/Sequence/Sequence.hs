{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  ELynx.Data.Sequence
Description :  Hereditary sequences
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct  4 18:54:51 2018.

This module is to be imported qualified.

-}

module ELynx.Data.Sequence.Sequence
  ( -- * Types
    Name
  , Description
  , Characters
  , Sequence(..)
  -- * Input
  , fromByteString
  -- * Output
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
  , filterStandard
  )
where

import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )
import qualified Data.Vector.Unboxed           as V
import           Prelude                 hiding ( concat
                                                , length
                                                )
import qualified Prelude                       as Pr
                                                ( length )
import qualified Text.Printf                   as P

import qualified ELynx.Data.Alphabet.Alphabet  as A
import           ELynx.Data.Alphabet.Character
import           ELynx.Data.Sequence.Defaults
import           ELynx.Tools

-- | For now, 'Name's are just 'L.ByteString's.
type Name = L.ByteString

-- | The description of a sequence.
type Description = L.ByteString

-- | The vector of characters of a sequence.
type Characters = V.Vector Character

-- | Convert byte string to sequence characters.
fromByteString :: L.ByteString -> Characters
fromByteString = V.fromList . map fromChar . L.unpack

-- | Convert sequence characters to byte string.
toByteString :: Characters -> L.ByteString
toByteString = L.pack . map toChar . V.toList

-- | Sequences have a name, a possibly empty description, a code and hopefully a
-- lot of data.
data Sequence = Sequence { name        :: Name
                         , description :: Description
                         , alphabet    :: A.Alphabet
                         , characters  :: Characters }
  deriving (Show, Eq)

getInfo :: Sequence -> L.ByteString
getInfo s = L.unwords
  [ alignLeft nameWidth (name s)
  , alignRight fieldWidth (L.pack $ show $ alphabet s)
  , alignRight fieldWidth (L.pack . show $ len)
  , alignRight fieldWidth (L.pack $ P.printf "%2.2f" pGaps)
  ]
 where
  len   = length s
  nGaps = countGaps s
  pGaps = 100 * fromIntegral nGaps / fromIntegral len :: Double

-- | Trim and show a 'Sequence'.
summarize :: Sequence -> L.ByteString
summarize s = L.unwords
  [getInfo s, summarizeByteString summaryLength $ toByteString (characters s)]

-- | Trim and show a list of 'Sequence's.
summarizeSequences :: [Sequence] -> L.ByteString
summarizeSequences ss = header ss <> body (take summaryNSequences ss)

-- | Header printed before 'Sequence' list.
tableHeader :: L.ByteString
tableHeader = L.unwords
  [ alignLeft nameWidth "Name"
  , alignRight fieldWidth "Code"
  , alignRight fieldWidth "Length"
  , alignRight fieldWidth "Gaps [%]"
  , "Sequence"
  ]

-- | A short description of the sequence.
header :: [Sequence] -> L.ByteString
header ss =
  L.unlines
    $  reportIfSubsetIsShown
    ++ [ L.pack
       $  "For each sequence, the "
       ++ show summaryLength
       ++ " first bases are shown."
       , L.pack $ "List contains " ++ show (Pr.length ss) ++ " sequences."
       , ""
       , tableHeader
       ]
 where
  l = Pr.length ss
  s =
    show summaryNSequences
      ++ " out of "
      ++ show (Pr.length ss)
      ++ " sequences are shown."
  reportIfSubsetIsShown | l > summaryNSequences = [L.pack s]
                        | otherwise             = []

-- | Trim and show a list of 'Sequence's.
body :: [Sequence] -> L.ByteString
body ss = L.unlines (map summarize ss `using` parListChunk 5 rdeepseq)

-- | Calculate length of 'Sequence'.
length :: Sequence -> Int
length = fromIntegral . V.length . characters

-- | Check if all 'Sequence's have equal length.
equalLength :: [Sequence] -> Bool
equalLength = allEqual . map length

-- | Find the longest 'Sequence' in a list.
longest :: [Sequence] -> Sequence
longest = maximumBy (comparing length)

-- | Count number of gaps or unknown characters in sequence.
countGaps :: Sequence -> Int
countGaps s = V.length . V.filter (A.isGap $ alphabet s) $ characters s

-- | Trim to given length.
trim :: Int -> Sequence -> Sequence
trim n (Sequence nm d a cs) = Sequence nm d a (V.take (fromIntegral n) cs)

-- | Concatenate two sequences. 'Name's have to match.
concat :: Sequence -> Sequence -> Sequence
concat (Sequence i d c cs) (Sequence j f k ks)
  | i /= j
  = error
    $  "concatenate: Sequences do not have equal names: "
    ++ L.unpack i
    ++ ", "
    ++ L.unpack j
    ++ "."
  | d /= f
  = error
    $  "concatenate: Sequences do not have equal descriptions: "
    ++ L.unpack d
    ++ ", "
    ++ L.unpack f
    ++ "."
  | c /= k
  = error
    $  "concatenate: Sequences do not have equal alphabets: "
    ++ show c
    ++ ", "
    ++ show k
    ++ "."
  | otherwise
  = Sequence i d c (cs <> ks)

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

-- | Only take 'Sequence's that contain at least on non-IUPAC character.
filterStandard :: [Sequence] -> [Sequence]
filterStandard = filter (\s -> anyStandard (alphabet s) s)

-- Are all characters IUPAC characters?
anyStandard :: A.Alphabet -> Sequence -> Bool
anyStandard a s = V.any (A.isStd a) cs where cs = characters s
