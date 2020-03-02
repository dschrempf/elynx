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
import qualified Data.Text                     as T

import           Tools
import           Translate.Options

import           ELynx.Data.Character.Codon
import           ELynx.Data.Sequence.Sequence
import           ELynx.Data.Sequence.Translate
import           ELynx.Export.Sequence.Fasta
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Reproduction       ( ELynx )

translateSeqs :: Int -> UniversalCode -> [Sequence] -> [Sequence]
translateSeqs rf uc = map (translateSeq uc rf)

-- | Translate sequences.
translateCmd :: TranslateArguments -> ELynx ()
translateCmd (TranslateArguments al inFile rf uc) = do
  $(logInfo) "Command: Translate sequences to amino acids."
  $(logInfo) $ T.pack $ "  Universal code: " <> show uc <> "."
  $(logInfo) $ T.pack $ "  Reading frame: " <> show rf <> "."
  $(logInfo) ""
  ss <- readSeqs al inFile
  let result = sequencesToFasta $ translateSeqs rf uc ss
  fn <- getOutFilePath ".fasta"
  out "translated sequences" result fn
