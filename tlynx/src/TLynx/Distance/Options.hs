{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  TLynx.Distance.Options
-- Description :  Options of tree-dist
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Aug 29 13:02:22 2019.
module TLynx.Distance.Options
  ( DistanceArguments (..),
    DistanceMeasure (..),
    distanceArguments,
    distanceFooter,
  )
where

import Data.Scientific (toRealFloat)
import Data.Void
import ELynx.Tools
import Options.Applicative
import TLynx.Parsers
import Text.Megaparsec
  ( Parsec,
    eof,
    try,
  )
import Text.Megaparsec.Char
  ( char,
    string,
  )
import Text.Megaparsec.Char.Lexer (scientific)
import Text.Printf

-- | Supported distance measures.
data DistanceMeasure
  = -- | Symmetric distance.
    Symmetric
  | -- | Incompatible split distance; collapse nodes
    -- with branch support below given value.
    IncompatibleSplit Double
  | -- | Branch score distance.
    BranchScore
  deriving (Eq, Generic)

instance FromJSON DistanceMeasure

instance ToJSON DistanceMeasure

instance Show DistanceMeasure where
  show Symmetric = "Symmetric"
  show (IncompatibleSplit c) = "Incompatible Split (" ++ printf "%.2f" c ++ ")"
  show BranchScore = "Branch Score"

-- | Arguments needed to compute distance measures.
data DistanceArguments = DistanceArguments
  { argsDistance :: DistanceMeasure,
    argsNormalize :: Bool,
    argsIntersect :: Bool,
    argsSummaryStatistics :: Bool,
    argsMasterTreeFile :: Maybe FilePath,
    argsNewickFormat :: NewickFormat,
    argsInFiles :: [FilePath]
  }
  deriving (Eq, Show, Generic)

instance Reproducible DistanceArguments where
  inFiles a = case argsMasterTreeFile a of
    Nothing -> argsInFiles a
    Just f -> f : argsInFiles a
  outSuffixes _ = [".out"]
  getSeed _ = Nothing
  setSeed = const
  parser = distanceArguments
  cmdName = "distance"
  cmdDsc = ["Compute distances between many phylogenetic trees."]
  cmdFtr = distanceFooter

instance FromJSON DistanceArguments

instance ToJSON DistanceArguments

-- | COmmand line parser.
distanceArguments :: Parser DistanceArguments
distanceArguments =
  DistanceArguments
    <$> distanceOpt
    <*> normalizeSwitch
    <*> intersectSwitch
    <*> summaryStatisticsSwitch
    <*> masterTreeFile
    <*> newickFormat
    <*> many inFilesArg

masterTreeFile :: Parser (Maybe FilePath)
masterTreeFile =
  optional $
    strOption $
      long "master-tree-file"
        <> short 'm'
        <> metavar "MASTER-TREE-File"
        <> help "Compare all trees to the tree in the master tree file."

inFilesArg :: Parser FilePath
inFilesArg =
  strArgument $
    metavar "INPUT-FILES"
      <> help
        "Read tree(s) from INPUT-FILES; if more files are given, one tree is expected per file"

symmetric :: Parsec Void String DistanceMeasure
symmetric = do
  _ <- string "symmetric"
  _ <- eof
  pure Symmetric

incompatibleSplit :: Parsec Void String DistanceMeasure
incompatibleSplit = do
  _ <- string "incompatible-split"
  _ <- char '['
  f <- toRealFloat <$> scientific
  _ <- char ']'
  _ <- eof
  if (0 <= f) && (f <= 1)
    then pure $ IncompatibleSplit f
    else error "Branch support has to be in [0, 1]."

branchScore :: Parsec Void String DistanceMeasure
branchScore = do
  _ <- string "branch-score"
  _ <- eof
  pure BranchScore

distanceParser :: Parsec Void String DistanceMeasure
distanceParser =
  -- Try first the normalized one, since the normal branch score
  -- parser also succeeds in this case.
  try symmetric <|> try incompatibleSplit <|> branchScore

distanceOpt :: Parser DistanceMeasure
distanceOpt =
  option (megaReadM distanceParser) $
    long "distance"
      <> short 'd'
      <> metavar "MEASURE"
      <> help
        "Type of distance to calculate (available distance measures are listed below)"

summaryStatisticsSwitch :: Parser Bool
summaryStatisticsSwitch =
  switch $
    long "summary-statistics" <> short 's'
      <> help
        "Report summary statistics only"

normalizeSwitch :: Parser Bool
normalizeSwitch =
  switch $
    long "normalize"
      <> short 'n'
      <> help
        "Normalize trees before distance calculation; only affect distances depending on branch lengths"

intersectSwitch :: Parser Bool
intersectSwitch =
  switch $
    long "intersect"
      <> short 't'
      <> help
        "Compare intersections; i.e., before comparison, drop leaves that are not present in the other tree"

-- | Information about provided distance types.
distanceFooter :: [String]
distanceFooter =
  [ "Distance measures:",
    "  symmetric                Symmetric distance (Robinson-Foulds distance).",
    "  incompatible-split[VAL]  Incompatible split distance. Collapse branches with (normalized)",
    "                           support less than 0.0<=VAL<=1.0 before distance calculation;",
    "                           if, let's say, VAL>0.7, only well supported differences contribute",
    "                           to the total distance.",
    "  branch-score             Branch score distance."
  ]
