{- |
Module      :  Examine.Options
Description :  Tree analysis options
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Aug 29 08:16:45 2019.

-}

module Examine.Options
  ( ExamineArguments (..)
  , Examine
  , examineArguments
  ) where

import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Options.Applicative

newtype ExamineArguments = ExamineArguments
  { inFile     :: Maybe FilePath }

type Examine = LoggingT (ReaderT ExamineArguments IO)

examineArguments :: Parser ExamineArguments
examineArguments = ExamineArguments <$> optional inFileArg

inFileArg :: Parser FilePath
inFileArg = strArgument $
  metavar "INPUT-FILE" <>
  help "Read trees from INPUT-FILE"
