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
import qualified Data.ByteString.Lazy.Char8                     as B
import           Data.Tree
import qualified Data.Vector                                    as V
import           System.Random.MWC

import           ArgParseSim
import           ParsePhyloModel

import           EvoMod.ArgParse
import           EvoMod.Data.Alphabet.Alphabet
-- import           EvoMod.Data.RateMatrix.Nucleotide
import           EvoMod.Data.MarkovProcess.PhyloModel
import           EvoMod.Data.MarkovProcess.RateMatrix
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.NamedTree
import           EvoMod.Export.Sequence.Fasta
import           EvoMod.Import.MarkovProcess.EDMModelPhylobayes hiding (Parser)
-- import           EvoMod.Data.Tree.PhyloTree
import           EvoMod.Data.Tree.Tree
import           EvoMod.Import.Tree.Newick                      hiding (name)
import           EvoMod.Simulate.MarkovProcessAlongTree
import           EvoMod.Tools

-- | Simulate a 'MultiSequenceAlignment' for a given substitution model and
-- phylogenetic tree.
simulateMSA :: (PrimMonad m, Measurable a, Named a)
            => Code -> Int -> RateMatrix -> Tree a -> Gen (PrimState m)
            -> m MultiSequenceAlignment
simulateMSA c n q t g = do
  statesTree <- simulateNSitesAlongTree n q t g
  let leafNames  = map name $ leafs t
      leafStates = leafs statesTree
      sequences  = [ toSequence sId (B.pack . map w2c $ indicesToCharacters c ss) |
                    (sId, ss) <- zip leafNames leafStates ]
  return $ fromSequenceList sequences

main :: IO ()
main = do
  EvoModSimArgs trFn pMs len mEdmF mSeed q outFn <- parseEvoModSimArgs
  unless q $ do
    programHeader
    putStrLn ""
    putStrLn "Read tree."
  tree <- parseFileWith newick trFn
  unless q $ do
    B.putStr $ summarize tree
    putStrLn ""
  edmCs <- case mEdmF of
    Nothing   -> return Nothing
    Just edmF -> do
      unless q $
        putStrLn "Read EDM file."
      Just <$> parseFileWith phylobayes edmF
  unless q $ do
    putStrLn "TODO: Summarize EDM components."
    putStrLn ""
    putStrLn "Read model string."
  let pM = parseByteStringWith (phyloModelString edmCs) pMs
  unless q $ do
    case pM of
      -- TODO: Output mixture model summary.
      PhyloMixtureModel _       -> undefined
      PhyloSubstitutionModel sm -> B.putStr $ summarizeSubstitutionModel sm
    putStrLn ""
    putStrLn "Simulate alignment."
    putStrLn $ "Length: " ++ show len ++ "."
  g <- case mSeed of
    Nothing -> putStrLn "Seed: random"
               >> createSystemRandom
    Just s  -> putStrLn ("Seed: " ++ show s ++ ".")
               >> initialize (V.fromList s)
  msa <- case pM of
    -- TODO: Handle mixture models.
    PhyloMixtureModel _ -> undefined
    PhyloSubstitutionModel sm -> simulateMSA (smCode sm) len (smRateMatrix sm) tree g
  let output = sequencesToFasta $ msaSequences msa
  B.writeFile outFn output
  unless q $ do
    putStrLn ""
    putStrLn ("Output written to file '" ++ outFn ++ "'.")
