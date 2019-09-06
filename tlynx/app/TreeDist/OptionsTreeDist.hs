{- |
Module      :  OptionsTreeDist
Description :  Options of tree-dist
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Aug 29 13:02:22 2019.

-}

module OptionsTreeDist
  ( CommandArguments (..)
  , Arguments (..)
  , Distance (..)
  , parseArguments
  ) where

import           Data.Void
import           Options.Applicative
import           Text.Megaparsec            (Parsec, try)
import           Text.Megaparsec.Char       (char, string)
import           Text.Megaparsec.Char.Lexer (float)

import           ELynx.Tools.Options

data Distance = Symmetric | IncompatibleSplit Double
  deriving (Show, Read)

data CommandArguments = CommandArguments
  { argsDistance          :: Distance
  , argsSummaryStatistics :: Bool
  }

commandArguments :: Parser CommandArguments
commandArguments = CommandArguments <$>
  distanceOpt
  <*> summaryStatisticsSwitch

data Arguments = Arguments
  { globalArgs  :: GlobalArguments
  , commandArgs :: CommandArguments }

arguments :: Parser Arguments
arguments = Arguments
  <$> globalArguments
  <*> commandArguments

-- XXX: For the moment, only one input file is take (consistency with other commands).

-- TODO: Make the input file part of the CommandArguments. This would also with
-- with the input file having different formats.

-- filePathArg :: Parser FilePath
-- filePathArg = strArgument $
--   metavar "INPUT-FILES" <>
--   help "Read tree(s) from INPUT-FILES; if more files are given, one tree is expected per file"

symmetric :: Parsec Void String Distance
symmetric = do
  _ <- string "symmetric"
  pure Symmetric

incompatibleSplit :: Parsec Void String Distance
incompatibleSplit = do
  _ <- string "incompatible-split"
  _ <- char '['
  f <- float
  _ <- char ']'
  if (0 < f) && (f < 1)
    then pure $ IncompatibleSplit f
    else error "Branch support has to be between 0 and 1."

distanceParser :: Parsec Void String Distance
distanceParser = try symmetric <|> incompatibleSplit

distanceOpt :: Parser Distance
distanceOpt = option (megaReadM distanceParser) $
  long "distance" <>
  short 'd' <>
  metavar "MEASURE" <>
  help "Type of distance to calculate (available distance measures are listed below)"

summaryStatisticsSwitch :: Parser Bool
summaryStatisticsSwitch = switch $
  long "summary-statistics" <>
  short 's' <>
  help "Report summary statistics only"

desc :: [String]
desc = [ "Compute distances between phylogenetic trees." ]

ftr :: [String]
ftr = [ "Available distance measures:"
      , "  Symmetric distance: -d symmetric"
      , "  Incompatible split distance: -d incompatible-split[VAL]"
      , "    Collapse branches with support less than VAL before distance calculation;"
      , "    in this way, only well supported difference contribute to the distance measure." ]

parseArguments :: IO Arguments
parseArguments = parseArgumentsWith desc ftr arguments
