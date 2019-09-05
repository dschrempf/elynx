{- |
Module      :  Options
Description :  SLynx options
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep  5 20:27:17 2019.

-}

module Options
  (
  ) where

import           Data.Word

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Tools.Options
import           ELynx.Data.Character.Codon

data Command = Examine
               { exPerSite       :: Bool
               , exMbFp          :: Maybe FilePath }
             | Concatenate
               { ccMbFps         :: [FilePath] }
             | FilterRows
               { frLonger        :: Maybe Int
               , frShorter       :: Maybe Int
               , frMbFp          :: Maybe FilePath }
             | FilterColumns
               { fcStandard      :: Maybe Double
               , fcMbFp          :: Maybe FilePath }
             | SubSample
               { ssNSites        :: Int
               , ssNAlignments   :: Int
               , ssMbSeed        :: Maybe [Word32]
               , ssMbFp          :: Maybe FilePath }
             | Translate
               { trReadingFrame  :: Int
               , trUniversalCode :: UniversalCode
               , trMbFp          :: Maybe FilePath }

data GlobalArgs = GlobalArgs
  { argsAlphabet    :: Alphabet
  , argsOutBaseName :: Maybe FilePath
  , argsVerbosity   :: Verbosity }

data Args = Args
  { argsGlobal  :: GlobalArgs
  , argsCommand :: Command
  }

args :: Parser Args
args = Args <$>
       globalArgs <*>
       commandArg

globalArgs :: Parser GlobalArgs
globalArgs = GlobalArgs <$>
             alphabetOpt <*>
             optional outFileBaseNameOpt <*>
             verbosityOpt

commandArg :: Parser Command
commandArg = hsubparser $
  examineCommand <>
  concatenateCommand <>
  filterRowsCommand <>
  filterColumnsCommand <>
  subSampleCommand <>
  translateCommand
