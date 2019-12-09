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

module Filter.Filter
  ( filterRowsCmd
  , filterColsCmd
  )
  where

import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8                 as L
import           Data.Maybe                                 (fromMaybe)
import qualified Data.Text                                  as T

import           Filter.Options
import           Tools

import qualified ELynx.Data.Sequence.Alignment as M
import qualified ELynx.Data.Sequence.Sequence               as S
import           ELynx.Export.Sequence.Fasta
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Misc

filterRows :: Maybe Int -> Maybe Int -> [S.Sequence] -> L.ByteString
filterRows ml ms ss = sequencesToFasta $ compose filters ss
  where filters = map (fromMaybe id) [S.filterLongerThan <$> ml, S.filterShorterThan <$> ms]

-- | Filter sequences.
filterRowsCmd :: Maybe FilePath -> FilterRows ()
filterRowsCmd outFileBaseName = do
  $(logInfo) "Command: Filter sequences of a list of sequences."
  FilterRowsArguments al inFile long short <- lift ask
  maybe (return ())
    (\val -> $(logInfo) $ T.pack $ "  Keep sequences longer than " <> show val <> ".") long
  maybe (return ())
    (\val -> $(logInfo) $ T.pack $ "  Keep sequences shorter than " <> show val <> ".") short
  ss <- readSeqs al inFile
  let result      = filterRows long short ss
  let outFilePath = (++ ".fasta") <$> outFileBaseName
  out "filtered sequences" result outFilePath

filterCols :: Maybe Double -> [S.Sequence] -> L.ByteString
filterCols ms ss = sequencesToFasta . M.toSequences $ compose filters a
  where a = either error id (M.fromSequences ss)
        filters = map (fromMaybe id) [ M.filterColsStd <$> ms ]

-- | Filter columns.
filterColsCmd :: Maybe FilePath -> FilterCols ()
filterColsCmd outFileBaseName = do
  $(logInfo) "Command: Filter columns of a multi sequence alignment."
  FilterColsArguments al inFile standard <- lift ask
  case standard of
    Nothing -> return ()
    Just p -> $(logInfo) $ T.pack $
        "  Keep columns with a proportion of standard (non-IUPAC) characters larger than "
        ++ show p ++ "."
  ss <- readSeqs al inFile
  let result      = filterCols standard ss
  let outFilePath = (++ ".fasta") <$> outFileBaseName
  out "filtered sequences" result outFilePath
