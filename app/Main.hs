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

import           Evol.ArgParse
import           Evol.Data.Sequence
import           Evol.IO.Fasta
import           Evol.Tools                       (parseFileWith)

summarizeSeqs :: [Sequence] -> String
summarizeSeqs = summarizeSequenceList

concatenateSeqs :: [[Sequence]] -> Either String [Sequence]
concatenateSeqs []   = Left "Nothing to concatenate."
concatenateSeqs [ss] = Left $ "Got only one list of sequences: " ++ summarizeSequenceList ss
concatenateSeqs sss  = foldM (zipWithM concatenate) (head sss) (tail sss)

act :: Command -> [[Sequence]] -> IO ()
act Summarize sss   = mapM_ (putStr . summarizeSeqs) sss
act Concatenate sss = case concatenateSeqs sss of
                       Left e   -> putStrLn e
                       Right ss -> putStrLn (summarizeSequenceList ss)

main :: IO ()
main = do (EvolIOArgs cmd c fns) <- parseEvolIOArgs
          putStrLn ""
          putStrLn "Read fasta file."
          putStrLn $ "Code: " ++ show c ++ "."
          -- 'sss' is a little bad, but IT IS a list of a list of sequences.
          sss <- sequence $ parseFileWith (fastaFile c) <$> fns
          act cmd sss
