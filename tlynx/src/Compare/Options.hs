{- |
Module      :  Compare.Options
Description :  Options of tree-dist
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Aug 29 13:02:22 2019.

-}

module Compare.Options
  ( CompareArguments (..)
  , Compare
  , Distance (..)
  , compareArguments
  , compareFooter
  ) where

import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.List
import           Data.Void
import           Options.Applicative
import           Text.Megaparsec            (Parsec, try)
import           Text.Megaparsec.Char       (char, string)
import           Text.Megaparsec.Char.Lexer (float)
import           Text.Printf

import           ELynx.Tools.Options

data Distance = Symmetric | IncompatibleSplit Double | BranchScore

instance Show Distance where
  show Symmetric             = "Symmetric"
  show (IncompatibleSplit c) = "Incompatible Split (" ++ printf "%.1f" c ++ ")"
  show BranchScore           = "Branch Score"

data CompareArguments = CompareArguments
  { argsDistance          :: Distance
  , argsSummaryStatistics :: Bool
  , argsInFiles           :: [FilePath]
  }

type Compare = LoggingT (ReaderT CompareArguments IO)

compareArguments :: Parser CompareArguments
compareArguments = CompareArguments <$>
  distanceOpt
  <*> summaryStatisticsSwitch
  <*> many inFilesArg

inFilesArg :: Parser FilePath
inFilesArg = strArgument $
  metavar "INPUT-FILES" <>
  help "Read tree(s) from INPUT-FILES; if more files are given, one tree is expected per file"

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

branchScore :: Parsec Void String Distance
branchScore = do
  _ <- string "branch-score"
  pure BranchScore

distanceParser :: Parsec Void String Distance
distanceParser = try symmetric <|> incompatibleSplit <|> branchScore

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

compareFooter :: String
compareFooter = intercalate "\n"
  [ "Available distance measures:"
  , "  Symmetric distance: -d symmetric"
  , "  Incompatible split distance: -d incompatible-split[VAL]"
  , "    Collapse branches with support less than VAL before distance calculation;"
  , "    in this way, only well supported difference contribute to the distance measure."
  , "  Branch score distance : -d branch-score" ]
