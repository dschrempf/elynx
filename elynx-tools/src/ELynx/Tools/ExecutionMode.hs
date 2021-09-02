{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  ELynx.Tools.ExecutionMode
-- Description :  Overwrite existing files or fail
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  2 19:07:32 2021.
module ELynx.Tools.ExecutionMode
  ( ExecutionMode (..),
    HasExecutionMode (..),
    openFile',
  )
where

import Data.Aeson
import GHC.Generics
import System.Directory (doesFileExist)
import System.IO

-- | Overwrite existing output files or fail if output files exist.
data ExecutionMode = Overwrite | Fail
  deriving (Eq, Show, Generic)

instance FromJSON ExecutionMode

instance ToJSON ExecutionMode

class HasExecutionMode a where
  getExecutionMode :: a -> ExecutionMode

checkFile :: ExecutionMode -> FilePath -> IO ()
checkFile Overwrite _ = return ()
checkFile Fail fp =
  doesFileExist fp >>= \case
    True ->
      error $
        "File exists: "
          <> fp
          <> "."
          <> "\n"
          <> "Please use --force to overwrite results of a previous analysis."
    False -> return ()

-- | Open existing files only if 'Force' is true.
openFile' :: ExecutionMode -> FilePath -> IOMode -> IO Handle
openFile' em fp md = checkFile em fp >> openFile fp md
