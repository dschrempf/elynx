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

import qualified Data.Attoparsec.Text            as A
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Options.Applicative
import           Options.Applicative.Help.Pretty

import           Base.Alphabet
import           Base.AminoAcid
import           Base.MultiSequenceAlignment
import           Base.Nucleotide
import           EvolIO.Fasta

data EvolIOArgs = EvolIOArgs
                  { fileName :: String
                  , alphabet :: AlphabetName }

evolIOOpts :: Parser EvolIOArgs
evolIOOpts = EvolIOArgs
  <$> fileNameOpt
  <*> alphabetOpt

fileNameOpt :: Parser String
fileNameOpt = option str
  ( long "input-file"
    <> short 'i'
    <> metavar "NAME"
    <> showDefault
    <> help "Read sequences from NAME." )

alphabetOpt :: Parser AlphabetName
alphabetOpt = option auto
  ( long "alphabet"
    <> short 'a'
    <> metavar "NAME"
    <> showDefault
    <> help "Alphabet type; DNA or AA." )

-- | Read the arguments and prints out help if needed.
parseEvolIOArgs :: IO EvolIOArgs
parseEvolIOArgs = execParser $
  info (helper <*> evolIOOpts)
  (fullDesc
    <> progDesc "Parse sequence file formats and analyze them."
    <> header "Evolutionary sequences."
    <> footerDoc formats )
  where
    formats = Just $ vcat $ map pretty strs
    strs   = [ "File formats:"
             , "  - FASTA"
             , ""
             , "Alphabet types:"
             , "  - DNA: Nucleotides"
             , "  - AA:  Amino acids"
             ]

analyzeNucleotideMSA :: MultiSequenceAlignment Nucleotide -> String
analyzeNucleotideMSA = showSummaryMSA

analyzeAminoAcidMSA :: MultiSequenceAlignment AminoAcid -> String
analyzeAminoAcidMSA = showSummaryMSA

parseInput :: Alphabet a => T.Text -> MultiSequenceAlignment a
parseInput input = either error id (A.parseOnly fasta input)

main :: IO ()
main = do (EvolIOArgs fn al) <- parseEvolIOArgs
          input <-  T.readFile fn
          case al of
            DNA -> do putStrLn "Read nucleotide multisequence alignment."
                      putStrLn $ analyzeNucleotideMSA $ parseInput input
            AA  -> do putStrLn "Read amino acid multisequence alignment."
                      putStrLn $ analyzeAminoAcidMSA $ parseInput input
