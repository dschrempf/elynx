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
import           Evol.Data.MultiSequenceAlignment
import           Evol.Data.Nucleotide
import           Evol.Data.Sequence
import           Evol.IO.Fasta
import           Evol.Tools                       (parseFileWith)

analyzeNucleotideMSA :: MultiSequenceAlignment String Nucleotide -> String
analyzeNucleotideMSA = summarizeMSA

analyzeNucleotideIUPAC :: [Sequence String NucleotideIUPAC] -> String
analyzeNucleotideIUPAC = summarizeSequenceList

analyzeAminoAcidMSA :: MultiSequenceAlignment String AminoAcid -> String
analyzeAminoAcidMSA = summarizeMSA

main :: IO ()
main = do (EvolIOArgs fn al) <- parseEvolIOArgs
          case al of
            -- XXX: Does different stuff for now!
            DNA -> do putStrLn "Read nucleotide multisequence alignment."
                      msa <- parseFileWith (fastaFileMSA @Nucleotide) fn
                      putStrLn $ analyzeNucleotideMSA msa
            DNA_IUPAC -> do putStrLn "Read nucleotide multisequence alignment (with IUPAC codes)."
                            msa <- parseFileWith (fastaFile @NucleotideIUPAC) fn
                            putStrLn $ analyzeNucleotideIUPAC msa
            AA  -> do putStrLn "Read amino acid multisequence alignment."
                      msa <- parseFileWith (fastaFileMSA @AminoAcid) fn
                      putStrLn $ analyzeAminoAcidMSA msa
