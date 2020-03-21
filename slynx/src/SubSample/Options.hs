{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  SubSample.Options
Description :  ELynxSeq argument parsing
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}

module SubSample.Options
  ( SubSampleArguments(..)
  , subSampleArguments
  , getOutSuffixes
  )
where

import           Options.Applicative
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Builder        as LT
import qualified Data.Text.Lazy.Builder.Int    as LT

import           Tools

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools

-- | Data structure holding the Command line arguments.
data SubSampleArguments = SubSampleArguments
    { ssAlphabet    :: Alphabet
    , ssInFile      :: FilePath
    , ssNSites      :: Int
    , ssNAlignments :: Int
    , ssMbSeed      :: Seed }
  deriving (Eq, Show, Generic)

-- | Get a given number of output file suffixes.
--
-- > getOutSuffixes 11 "fasta"
--
-- Will result in @.00.fasta@ up to @.10.fasta@.
getOutSuffixes :: Int -> String -> [String]
getOutSuffixes n suffix =
  [ "." ++ digitStr i ++ "." ++ suffix | i <- [0 .. n - 1] ]
 where
  nDigits = ceiling $ logBase (10 :: Double) (fromIntegral n)
  digitStr i = T.unpack
    $ T.justifyRight nDigits '0' (LT.toStrict $ LT.toLazyText $ LT.decimal i)


instance Reproducible SubSampleArguments where
  inFiles = pure . ssInFile
  outSuffixes a = getOutSuffixes (ssNAlignments a) "fasta"
  getSeed = Just . ssMbSeed
  setSeed a s = a { ssMbSeed = Fixed s }
  parser  = subSampleArguments
  cmdName = "sub-sample"
  cmdDesc = "Sub-sample columns from multi sequence alignments."
  cmdFtr =
    Just
      "Create a given number of multi sequence alignments, each of which contains a given number of random sites drawn from the original multi sequence alignment."

instance ToJSON SubSampleArguments

-- | Sub command parser.
subSampleArguments :: Parser SubSampleArguments
subSampleArguments =
  SubSampleArguments
    <$> alphabetOpt
    <*> filePathArg
    <*> subSampleNSitesOpt
    <*> subSampleNAlignmentsOpt
    <*> seedOpt

subSampleNSitesOpt :: Parser Int
subSampleNSitesOpt =
  option auto $ long "number-of-sites" <> short 'n' <> metavar "INT" <> help
    "Number of sites randomly drawn with replacement"

subSampleNAlignmentsOpt :: Parser Int
subSampleNAlignmentsOpt =
  option auto
    $  long "number-of-alignments"
    <> short 'm'
    <> metavar "INT"
    <> help "Number of multi sequence alignments to be created"

filePathArg :: Parser FilePath
filePathArg =
  strArgument $ metavar "INPUT-FILE" <> help "Read sequences from INPUT-FILE"
