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

import           Evol.ArgParse
import           Evol.Data.Alphabets
import           Evol.Data.MultiSequenceAlignment
import           Evol.Data.Sequence
import           Evol.IO.Fasta
import           Evol.Tools                       (parseFileWith)

analyzeSequenceList :: [Sequence] -> String
-- analyzeSequenceList = summarizeSequenceList
analyzeSequenceList = summarizeMSA . fromSequenceList

main :: IO ()
main = do (EvolIOArgs fn an) <- parseEvolIOArgs
          putStrLn ""
          putStrLn "Read fasta file."
          putStrLn $ "Alphabet name: " ++ show an ++ "."
          let a = alphabet an
          ss <- parseFileWith (fastaFile a) fn
          putStr $ analyzeSequenceList ss
