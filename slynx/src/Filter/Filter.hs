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
  , filterColumnsCmd
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

import           ELynx.Data.Sequence.MultiSequenceAlignment
import           ELynx.Data.Sequence.Sequence
import           ELynx.Export.Sequence.Fasta
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Misc

filterRows :: Maybe Int -> Maybe Int -> [Sequence] -> L.ByteString
filterRows ml ms ss = sequencesToFasta $ compose filters ss
  where filters = map (fromMaybe id) [filterLongerThan <$> ml, filterShorterThan <$> ms]

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
  io "filtered sequences" result outFilePath

filterColumns :: Maybe Double -> [Sequence] -> L.ByteString
filterColumns ms ss = sequencesToFasta . toSequenceList $ compose filters msa
  where msa = either error id (fromSequenceList ss)
        filters = map (fromMaybe id) [ filterColumnsStd <$> ms ]

filterColumnsCmd :: Maybe FilePath -> FilterColumns ()
filterColumnsCmd outFileBaseName = do
  $(logInfo) "Command: Filter columns of a multi sequence alignment."
  FilterColumnsArguments al inFile standard <- lift ask
  case standard of
    Nothing -> return ()
    Just p -> $(logInfo) $ T.pack $
        "  Keep columns with a proportion of standard (non-IUPAC) characters larger than "
        ++ show p ++ "."
  ss <- readSeqs al inFile
  let result      = filterColumns standard ss
  let outFilePath = (++ ".fasta") <$> outFileBaseName
  io "filtered sequences" result outFilePath
