{- |
Module      :  Translate.Options
Description :  ELynxSeq argument parsing
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

-}

module Translate.Options
  ( TranslateArguments (..)
  , translateArguments
  ) where

import           Control.Applicative
import           Data.List
import           Options.Applicative

import           Tools

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Data.Character.Codon
import           ELynx.Tools.Misc

-- | Arguments needed to translate sequences.
data TranslateArguments = TranslateArguments
    { trAlphabet      :: Alphabet
    , trInFile        :: Maybe FilePath
    , trReadingFrame  :: Int
    , trUniversalCode :: UniversalCode }

-- | Command line parser.
translateArguments :: Parser TranslateArguments
translateArguments = TranslateArguments <$>
                     alphabetOpt <*>
                     optional inFileArg <*>
                     readingFrameOpt <*>
                     universalCodeOpt

readingFrameOpt :: Parser Int
readingFrameOpt = option auto $
  long "reading-frame" <>
  short 'r' <>
  metavar "INT" <>
  help "Reading frame [0|1|2]."

universalCodeOpt :: Parser UniversalCode
universalCodeOpt = option auto $
  long "universal-code" <>
  short 'u' <>
  metavar "CODE" <>
  help ("universal code; one of: " ++ codeStr ++ ".")
  where codes = allValues :: [UniversalCode]
        codeWords = map show codes
        codeStr = intercalate ", " codeWords

inFileArg :: Parser FilePath
inFileArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read sequences from INPUT-FILE"
