{- |
Module      :  Main
Description :  Parse sequence file formats and analyze them.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 08:41:05 2018.

TODO: Read from STDIN.
TODO: Verbosity.

-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B

import           Evol.ArgParse
import           Evol.Data.Sequence
import           Evol.IO.Fasta
import           Evol.Tools                       (parseFileWith)

concatenateSeqs :: [[Sequence]] -> Either String [Sequence]
concatenateSeqs []   = Left "Nothing to concatenate."
concatenateSeqs [ss] = Left $ "Got only one list of sequences: " ++ summarizeSequenceList ss
concatenateSeqs sss  = foldM (zipWithM concatenate) (head sss) (tail sss)

act :: Command -> [[Sequence]] -> Either String B.ByteString
act Summarize sss   = Right . B.pack $ concatMap summarizeSequenceList sss
act Concatenate sss = sequencesToFasta <$> concatenateSeqs sss

io :: Maybe String -> Either String B.ByteString-> IO ()
io _        (Left  s) = putStrLn s
io Nothing  (Right b) = B.putStr b
io (Just f) (Right b) = B.writeFile f b

main :: IO ()
main = do (EvolIOArgs cmd c mofn q fns) <- parseEvolIOArgs
          unless q $ do
            putStrLn "Read fasta file."
            putStrLn $ "Code: " ++ show c ++ "."
          -- 'sss' is a little weird, but IT IS a list of a list of sequences.
          sss <- sequence $ parseFileWith (fastaFile c) <$> fns
          let eRes = act cmd sss
          io mofn eRes
