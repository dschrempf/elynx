{- |
Module      :  Main
Description :  Parse sequence file formats and analyze them.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 08:41:05 2018.

-}

module Main where

import           Control.Monad
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
import           EvoMod.Tools.Misc

act :: Command -> [[Sequence]] -> Either L.ByteString L.ByteString
act Summarize sss      = Right . L.intercalate (L.pack "\n") $ map summarizeSequenceList sss
act Concatenate sss    = sequencesToFasta <$> concatenateSeqs sss
act (Filter ml ms) sss = Right . sequencesToFasta $ compose filters $ concat sss
  where filters        = map (fromMaybe id) [ filterLongerThan <$> ml
                                    , filterShorterThan <$> ms ]
act Analyze sss        = Right . L.intercalate (L.pack "\n") $ map (L.pack . show . kEffAll . toFrequencyData) msas
  where msas = map fromSequenceList sss

io :: Either L.ByteString L.ByteString -> Handle -> IO ()
io (Left  s)   _ = L.putStrLn s
io (Right res) h = L.hPutStr h res

main :: IO ()
main = do (EvoModSeqArgs cmd c mofn q fns) <- parseEvoModSeqArgs
          unless q $ do
            programHeader
            putStrLn "Read fasta file(s)."
            putStrLn $ "Code: " ++ show c ++ "."
            putStrLn ""
          -- 'sss' is a little weird, but it is a list of a list of sequences.
          sss <- sequence $ parseFileWith (fasta c) <$> fns
          let eRes = act cmd sss
          case mofn of
            Nothing -> io eRes stdout
            Just fn -> do
              withFile fn WriteMode (io eRes)
              unless q $ putStrLn ("Results written to file '" ++ fn ++ "'.")
