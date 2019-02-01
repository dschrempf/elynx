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
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe                    (fromMaybe)
import           System.IO

import           ArgParseSeq

import           EvoMod.ArgParse
import           EvoMod.Data.Sequence.Filter
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Export.Sequence.Fasta
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools                  (compose, parseFileWith)

concatenateSeqs :: [[Sequence]] -> Either B.ByteString [Sequence]
concatenateSeqs []   = Left $ B.pack "Nothing to concatenate."
concatenateSeqs [ss] = Left $ B.pack "Got only one list of sequences: " <> summarizeSequenceList ss
concatenateSeqs sss  = foldM (zipWithM concatenate) (head sss) (tail sss)

act :: Command -> [[Sequence]] -> Either B.ByteString B.ByteString
act Summarize sss    = Right . B.concat $ map summarizeSequenceList sss
act Concatenate sss  = sequencesToFasta <$> concatenateSeqs sss
act (Filter ml ms) sss = Right . sequencesToFasta $ compose filters $ concat sss
  where filters = map (fromMaybe id) [ filterLongerThan <$> ml
                                    , filterShorterThan <$> ms ]

io :: Either B.ByteString B.ByteString -> Handle -> IO ()
io (Left  s)   _ = B.putStrLn s
io (Right res) h = B.hPutStr h res

main :: IO ()
main = do (EvoModSeqArgs cmd c mofn q fns) <- parseEvoModSeqArgs
          unless q $ do
            programHeader
            putStrLn "Read fasta file."
            putStrLn $ "Code: " ++ show c ++ "."
            putStrLn ""
          -- 'sss' is a little weird, but it is a list of a list of sequences.
          sss <- sequence $ parseFileWith (fasta c) <$> fns
          let eRes = act cmd sss
          case mofn of
            Nothing -> io eRes stdout
            Just fn -> do
              unless q $ putStrLn ("Results written to file '" ++ fn ++ "'.")
              withFile fn WriteMode (io eRes)
