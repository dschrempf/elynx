{-# LANGUAGE DeriveGeneric       #-}

{- |
Module      :  Options
Description :  Options for elynx validation and redo sub commands
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Apr 23 19:17:17 2020.

-}

module Options
  ( AllReproductions(..)
  , getProgName
  , getArgs
  , ValidateArguments(..)
  , RedoArguments(..)
  , CommandArguments(..)
  , commandArguments
  ) where

import Options.Applicative

import ELynx.Tools

import  qualified         SLynx.Options as S
import  qualified         TLynx.Options as T

-- | Collect all reproductions in one data type.
data AllReproductions = S (Reproduction (Arguments S.CommandArguments))
                      | T (Reproduction (Arguments T.CommandArguments))

-- | Get program name irrespective of which program has been run.
getProgName :: AllReproductions -> String
getProgName (S x) = progName x
getProgName (T x) = progName x

-- | Get arguments irrespective of which program has been run.
getArgs :: AllReproductions -> [String]
getArgs (S x) = argsStr x
getArgs (T x) = argsStr x

newtype ValidateArguments = ValidateArguments
  { vElynxFile :: FilePath }
  deriving (Eq, Show, Generic)

validateArguments :: Parser ValidateArguments
validateArguments = ValidateArguments <$> inFileArg

validateDsc :: [String]
validateDsc = ["Validate a past ELynx analysis"]

newtype RedoArguments = RedoArguments
  { rElynxFile :: FilePath }
  deriving (Eq, Show, Generic)

redoArguments :: Parser RedoArguments
redoArguments = RedoArguments <$> inFileArg

redoDsc :: [String]
redoDsc = ["Redo a past ELynx analysis"]

inFileArg :: Parser FilePath
inFileArg = strArgument $ metavar "ELYNX-FILE"

data CommandArguments = Validate ValidateArguments
                      | Redo RedoArguments

validateCommand :: Mod CommandFields CommandArguments
validateCommand = createCommand "validate" validateDsc [] validateArguments Validate

redoCommand :: Mod CommandFields CommandArguments
redoCommand = createCommand "redo" redoDsc [] redoArguments Redo

commandArguments :: ParserInfo CommandArguments
commandArguments = elynxParserInfo [] [] $ hsubparser $ validateCommand <> redoCommand
