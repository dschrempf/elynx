{- |
Module      :  Main
Description :  Simulate multiple sequence alignments
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

TODO: Rate heterogeneity with Gamma distribution.

Creation date: Mon Jan 28 14:12:52 2019.

-}

module Main where

import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.ByteString.Lazy                        as B
import           Data.Tree
import qualified Data.Vector                                 as V
import           System.Environment
import           System.Random.MWC

import           ArgParseSim

import           EvoMod.ArgParse
import           EvoMod.Data.Alphabet.Alphabet
-- import           EvoMod.Data.RateMatrix.Nucleotide
import           EvoMod.Data.RateMatrix.RateMatrix
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.NamedTree
import           EvoMod.Export.Sequence.Fasta
-- import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Data.Tree.Tree
import           EvoMod.Import.Tree.Newick                   hiding (name)
import           EvoMod.Simulate.MarkovProcessAlongTree
import           EvoMod.Tools

-- | Simulate a 'MultiSequenceAlignment' for a given substitution model and
-- phylogenetic tree.
simulateMSA :: (PrimMonad m, Measurable a, Named a)
            => Code -> Int -> RateMatrix -> Tree a -> Gen (PrimState m)
            -> m MultiSequenceAlignment
simulateMSA c n q t g = do
  statesTree <- simulateNSitesAlongTree n q t g
  let leafNames = map name $ leafs t
      leafStates = leafs statesTree
      sequences = [ toSequence i (B.pack $ indicesToCharacters c ss) |
                    (i, ss) <- zip leafNames leafStates ]
  return $ fromSequenceList sequences

main :: IO ()
main = do (EvoModSimArgs trFn q outFn sm len mSeed) <- parseEvoModSimArgs
          unless q $ do
            p  <- getProgName
            as <- getArgs
            putStrLn evoModHeader
            putStrLn $ "Command line: " ++ p ++ " " ++ unwords as
            putStrLn ""
            putStrLn "Read tree."
          tree <- parseFileWith newick trFn
          unless q $ do
            -- TODO: Summarize tree (library function to write, call here).
            -- putStrLn $ show tree
            putStrLn ""
            putStrLn "Substitution model:"
            putStrLn $ "Code: " ++ show (mCode sm) ++ "."
            putStrLn $ "Name: " ++ show (mName sm) ++ "."
            putStrLn $ "Parameters: " ++ show (mParams sm) ++ "."
            -- XXX: This will be very verbose with amino acids or codons.
            putStrLn $ "Rate matrix: " ++ show (mRateMatrix sm) ++ "."
            putStrLn ""
            putStrLn "Simualte alignment."
            putStrLn $ "Length: " ++ show len ++ "."
          g <- case mSeed of
               Nothing -> createSystemRandom
               Just s  -> initialize (V.fromList s)
          case mSeed of
            Nothing -> putStrLn "Seed: random."
            Just s  -> putStrLn $ "Seed: " ++ show s ++ "."
          msa <- simulateMSA (mCode sm) len (mRateMatrix sm) tree g
          let output = sequencesToFasta $ msaSequences msa
          B.writeFile outFn output
          unless q $ putStrLn ("Output written to file '" ++ outFn ++ "'.")
