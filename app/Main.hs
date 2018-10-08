{- |
Module      :  Main
Description :  Parse sequence file formats and analyze them.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 08:41:05 2018.

TODO: Check if all records have correct name.

-}


module Main where

import qualified Data.Text.IO                as T
import           Text.Megaparsec

import           Evol.ArgParse
import           Evol.Data.Alphabet
import           Evol.Data.AminoAcid
import           Evol.Data.MultiSequenceAlignment
import           Evol.Data.Nucleotide
import           Evol.IO.Fasta

analyzeNucleotideMSA :: MultiSequenceAlignment String Nucleotide -> String
analyzeNucleotideMSA = showSummaryMSA

analyzeAminoAcidMSA :: MultiSequenceAlignment String AminoAcid -> String
analyzeAminoAcidMSA = showSummaryMSA

parseFile :: Alphabet a => String -> IO (MultiSequenceAlignment String a)
parseFile fn = do res <- parse fastaMSA fn <$> T.readFile fn
                  case res of
                    Left  err -> error $ parseErrorPretty err
                    Right msa -> return msa

main :: IO ()
main = do (EvolIOArgs fn al) <- parseEvolIOArgs
          case al of
            DNA -> do putStrLn "Read nucleotide multisequence alignment."
                      msa <- parseFile fn
                      putStrLn $ analyzeNucleotideMSA msa
            AA  -> do putStrLn "Read amino acid multisequence alignment."
                      msa <- parseFile fn
                      putStrLn $ analyzeAminoAcidMSA msa
