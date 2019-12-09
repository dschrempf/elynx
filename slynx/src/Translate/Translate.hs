{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Analyze.Analyze
Description :  Parse sequence file formats and analyze them
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 08:41:05 2018.

-}

module Translate.Translate
  ( translateCmd
  )
  where

import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.Text                     as T

import           Tools
import           Translate.Options

import           ELynx.Data.Character.Codon
import           ELynx.Data.Sequence.Sequence
import           ELynx.Data.Sequence.Translate
import           ELynx.Export.Sequence.Fasta
import           ELynx.Tools.InputOutput

translateSeqs :: Int -> UniversalCode -> [Sequence] -> [Sequence]
translateSeqs rf uc = map (translateSeq uc rf)

-- | Translate sequences.
translateCmd :: Maybe FilePath -> Translate ()
translateCmd outFileBaseName = do
  $(logInfo) "Command: Translate sequences to amino acids."
  TranslateArguments al inFile rf uc <- lift ask
  $(logInfo) $ T.pack $ "  Universal code: " <> show uc <> "."
  $(logInfo) $ T.pack $ "  Reading frame: " <> show rf <> "."
  $(logInfo) ""
  ss <- readSeqs al inFile
  let result      = sequencesToFasta $ translateSeqs rf uc ss
  let outFilePath = (++ ".fasta") <$> outFileBaseName
  out "translated sequences" result outFilePath
