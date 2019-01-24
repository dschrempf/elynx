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
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import           ArgParse

import           EvoMod.Data.Sequence.Filter
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Export.Sequence.Fasta
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools                  (compose, parseFileWith)

concatenateSeqs :: [[Sequence]] -> Either String [Sequence]
concatenateSeqs []   = Left "Nothing to concatenate."
concatenateSeqs [ss] = Left $ "Got only one list of sequences: " ++ T.unpack (summarizeSequenceList ss)
concatenateSeqs sss  = foldM (zipWithM concatenate) (head sss) (tail sss)

act :: Command -> [[Sequence]] -> Either String B.ByteString
act Summarize sss    = Right . B.fromStrict .  T.encodeUtf8 $ T.concat $ map summarizeSequenceList sss
act Concatenate sss  = sequencesToFasta <$> concatenateSeqs sss
act (Filter ml ms) sss = Right . sequencesToFasta $ compose filters $ concat sss
  where filters = map (fromMaybe id) [ filterLongerThan <$> ml
                                    , filterShorterThan <$> ms ]

io :: Maybe String -> Either String B.ByteString-> IO ()
io _        (Left  s) = putStrLn s
io Nothing  (Right b) = B.putStr b
io (Just f) (Right b) = B.writeFile f b

main :: IO ()
main = do (EvoModIOArgs cmd c mofn q fns) <- parseEvoModIOArgs
          unless q $ do
            putStrLn evoModHeader
            putStrLn "Read fasta file."
            putStrLn $ "Code: " ++ show c ++ "."
          -- 'sss' is a little weird, but IT IS a list of a list of sequences.
          sss <- sequence $ parseFileWith (fastaFile c) <$> fns
          let eRes = act cmd sss
          io mofn eRes
          unless q $ putStrLn ""
