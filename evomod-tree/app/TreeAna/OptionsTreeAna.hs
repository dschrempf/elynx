{- |
Module      :  OptionsTreeAna
Description :  Tree analysis options
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Aug 29 08:16:45 2019.

-}

module OptionsTreeAna
  ( Args (..)
  , parseArgs
  ) where

import           Options.Applicative

import           EvoMod.Tools.Options

data Args = Args
  { argsOutBaseName :: Maybe FilePath
  , argsVerbosity   :: Verbosity }

args :: Parser Args
args = Args <$>
       optional outFileBaseNameOpt <*>
       verbosityOpt

parseArgs :: IO Args
parseArgs = parseArgsWith Nothing Nothing args
