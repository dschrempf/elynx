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

module SLynx.Translate.Translate
  ( translateCmd
  )
where

import           Control.Monad.Logger
import           Control.Monad.Trans.Reader     ( ask )
import qualified Data.Text                     as T

import           SLynx.Tools
import           SLynx.Translate.Options

import           ELynx.Data.Character.Codon
import           ELynx.Data.Sequence.Sequence
import           ELynx.Data.Sequence.Translate
import           ELynx.Export.Sequence.Fasta
import           ELynx.Tools

translateSeqs :: Int -> UniversalCode -> [Sequence] -> [Sequence]
translateSeqs rf uc = map (translateSeq uc rf)

-- | Translate sequences.
translateCmd :: ELynx TranslateArguments ()
translateCmd = do
  (TranslateArguments al inFile rf uc) <- local <$> ask
  $(logInfo) "Command: Translate sequences to amino acids."
  $(logInfo) $ T.pack $ "  Universal code: " <> show uc <> "."
  $(logInfo) $ T.pack $ "  Reading frame: " <> show rf <> "."
  $(logInfo) ""
  ss <- readSeqs al inFile
  let result = sequencesToFasta $ translateSeqs rf uc ss
  out "translated sequences" result "fasta"
