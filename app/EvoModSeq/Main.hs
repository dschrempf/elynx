{- |
Module      :  Main
Description :  Parse sequence file formats and analyze them.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 08:41:05 2018.

XXX: Somehow this implementation still uses 2.5 times the memory that it
actually needs to use. I think that when parsing the sequences, the lines are
copied into the complete sequence (see the function 'fastaSequence').

XXX: Provide possibility to parse and handle sequences with different codes.

-}

module Main where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8                  as L
import           Data.Maybe                                  (fromMaybe)
import           System.IO

import           ArgParseSeq

import           EvoMod.ArgParse
import           EvoMod.Data.Sequence.Filter
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Export.Sequence.Fasta
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.InputOutput
import           EvoMod.Tools.Logger
import           EvoMod.Tools.Misc

data Params = Params { arguments  :: EvoModSeqArgs
                     , mLogHandle :: Maybe Handle }

instance Logger Params where
  quiet = argsQuiet . arguments
  mHandle = mLogHandle

type Seq = ReaderT Params IO

act :: Command -> [[Sequence]] -> Either L.ByteString L.ByteString
act Summarize sss      = Right . L.intercalate (L.pack "\n") $ map summarizeSequenceList sss
act Concatenate sss    = sequencesToFasta <$> concatenateSeqs sss
act (Filter ml ms) sss = Right . sequencesToFasta $ compose filters $ concat sss
  where filters        = map (fromMaybe id) [ filterLongerThan <$> ml
                                    , filterShorterThan <$> ms ]
act Analyze sss        = Right . L.intercalate (L.pack "\n") $ map (L.pack . show . kEffAll . toFrequencyData) msas
  where msas = map fromSequenceList sss

io :: Either L.ByteString L.ByteString -> Seq ()
io (Left  s)   = logLBS s
io (Right res) = do
  mFileOut <- argsMaybeFileNameOut . arguments <$> ask
  case mFileOut of
    Nothing -> logLBSForce res
    Just fn -> do
      lift $ withFile fn WriteMode (`L.hPutStr` res)
      logS $ "Results written to file '" ++ fn ++ "'."

work :: Seq ()
work = do
  args <- arguments <$> ask
  header <- lift programHeader
  logS header
  let c = argsCode args
  logS $ "Read fasta file(s); code " ++ show c ++ "."
  logS ""
  let fns = argsFileNames args
  -- 'sss' is a little weird, but it is a list of a list of sequences.
  sss <- lift $ sequence $ parseFileWith (fasta c) <$> fns
  let eRes = act (argsCommand args) sss
  io eRes

main :: IO ()
main = do
  a <- parseEvoModSeqArgs
  h <- setupLogger (argsQuiet a) (argsMaybeFileNameOut a)
  runReaderT work (Params a h)
  closeLogger h
