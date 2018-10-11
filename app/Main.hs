{-# LANGUAGE TypeApplications #-}

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
import           Evol.Data.Alphabet
import           Evol.Data.AminoAcid
import           Evol.Data.Nucleotide
import           Evol.Data.Sequence
import           Evol.IO.Fasta
import           Evol.Tools                       (parseFileWith)

analyzeNucleotide :: [Sequence String Nucleotide] -> String
analyzeNucleotide = summarizeSequenceList

analyzeNucleotideIUPAC :: [Sequence String NucleotideIUPAC] -> String
analyzeNucleotideIUPAC = summarizeSequenceList

analyzeAminoAcid :: [Sequence String AminoAcid] -> String
analyzeAminoAcid = summarizeSequenceList

main :: IO ()
main = do (EvolIOArgs fn al) <- parseEvolIOArgs
          case al of
            DNA       -> do putStrLn "Read nucleotide multisequence alignment."
                            ss <- parseFileWith (fastaFile @Nucleotide) fn
                            putStrLn $ analyzeNucleotide ss
            DNA_IUPAC -> do putStrLn "Read nucleotide multisequence alignment (with IUPAC codes)."
                            msa <- parseFileWith (fastaFile @NucleotideIUPAC) fn
                            putStrLn $ analyzeNucleotideIUPAC msa
            AA        -> do putStrLn "Read amino acid multisequence alignment."
                            ss <- parseFileWith (fastaFile @AminoAcid) fn
                            putStrLn $ analyzeAminoAcid ss
