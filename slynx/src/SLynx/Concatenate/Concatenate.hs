{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Analyze.Analyze
-- Description :  Parse sequence file formats and analyze them
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 08:41:05 2018.
module SLynx.Concatenate.Concatenate
  ( concatenateCmd,
  )
where

import Control.Monad.Trans.Reader
import qualified ELynx.Data.Sequence.Sequence as S
import ELynx.Export.Sequence.Fasta
import ELynx.Tools
import SLynx.Concatenate.Options
import SLynx.Tools

-- | Concatenate sequences.
concatenateCmd :: ELynx ConcatenateArguments ()
concatenateCmd = do
  (ConcatenateArguments al fps) <- reader localArguments
  sss <- mapM (readSeqs al) fps
  let result = sequencesToFasta $ S.concatSequences sss
  out "concatenated multi sequence alignment " result ".fasta"
