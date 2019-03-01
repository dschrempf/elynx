{- |
Module      :  Main
Description :  Simulate multiple sequence alignments
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon Jan 28 14:12:52 2019.

-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString.Lazy                             as L
import qualified Data.ByteString.Lazy.Char8                       as LC
import           Data.Tree
import qualified Data.Vector                                      as V
import           Numeric.LinearAlgebra
import           System.Random.MWC

import           ArgParseSim
import           ParsePhyloModel

import           EvoMod.ArgParse
import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.GammaRateHeterogeneity
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.PhyloModel
import           EvoMod.Data.MarkovProcess.SubstitutionModel
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.NamedTree
import           EvoMod.Data.Tree.Tree
import           EvoMod.Export.Sequence.Fasta
import           EvoMod.Import.MarkovProcess.EDMModelPhylobayes   hiding
                                                                   (Parser)
import           EvoMod.Import.Tree.Newick                        hiding (name)
import           EvoMod.Simulate.MarkovProcessAlongTree
import           EvoMod.Tools.InputOutput
import           EvoMod.Tools.Misc

-- For a given number of capabilities and values, get the chunk sizes.
getChunks :: Int -> Int -> [Int]
getChunks c n = ns
  where n'  = n `div` c
        r = n `mod` c
        ns = replicate r (n'+1) ++ replicate (c - r) n'

-- | Simulate a 'MultiSequenceAlignment' for a given phylogenetic model,
-- phylogenetic tree, and alignment length.
simulateMSA :: (Measurable a, Named a)
            => PhyloModel -> Tree a -> Int -> GenIO
            -> IO MultiSequenceAlignment
simulateMSA pm t n g = do
  c  <- getNumCapabilities
  gs <- splitGen c g
  let chunks = getChunks c n
  leafStatesS <- case pm of
    PhyloSubstitutionModel sm -> mapConcurrently
      (\(num, gen) -> simulateAndFlattenNSitesAlongTree num (smRateMatrix sm) t gen)
      (zip chunks gs)
    PhyloMixtureModel mm      -> mapConcurrently
      (\(num, gen) -> simulateAndFlattenNSitesAlongTreeMixtureModel num ws qs t gen)
      (zip chunks gs)
      where
        ws = vector $ getWeights mm
        qs = getRateMatrices mm
  -- XXX: The horizontal concatenation might be slow. If so, 'concatenateSeqs'
  -- or 'concatenateMSAs' can be used, which directly appends vectors.
  let leafStates = horizontalConcat leafStatesS
      leafNames  = map name $ leafs t
      code       = pmCode pm
      sequences  = [ toSequence sName code (L.pack $ indicesToCharacters code ss) |
                    (sName, ss) <- zip leafNames leafStates ]
  return $ fromSequenceList sequences

-- | Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> L.ByteString
summarizeEDMComponents cs = LC.pack
                            $ "Empiricial distribution mixture model with "
                            ++ show (length cs) ++ " components"

-- TODO: Use ST (or Reader) to handle arguments, this will be especially useful
-- with 'phyloModelStr'.

-- TODO: Proper log.

main :: IO ()
main = do
  EvoModSimArgs treeFile phyloModelStr len mEDMFile mWs mGammaPs mSeed quiet outFile <- parseEvoModSimArgs
  unless quiet $ do
    programHeader
    putStrLn ""
    putStrLn "Read tree."
  tree <- parseFileWith newick treeFile
  unless quiet $
    L.putStr $ summarize tree
  edmCs <- case mEDMFile of
    Nothing   -> return Nothing
    Just edmF -> do
      unless quiet $ do
        putStrLn ""
        putStrLn "Read EDM file."
      Just <$> parseFileWith phylobayes edmF
  unless quiet $ do
    -- Is there a better way?
    maybe (return ()) (LC.putStrLn . summarizeEDMComponents) edmCs
    putStrLn ""
    putStrLn "Read model string."
  let phyloModel' = parseByteStringWith (phyloModelString edmCs mWs) phyloModelStr
      phyloModel = case mGammaPs of
        Nothing         -> phyloModel'
        Just (n, alpha) -> expand n alpha phyloModel'
  unless quiet $ do
    L.putStr . LC.unlines $ pmSummarize phyloModel
    putStrLn ""
    putStrLn "Simulate alignment."
    putStrLn $ "Length: " ++ show len ++ "."
  gen <- case mSeed of
    Nothing -> putStrLn "Seed: random"
               >> createSystemRandom
    Just s  -> putStrLn ("Seed: " ++ show s ++ ".")
               >> initialize (V.fromList s)
  msa <- simulateMSA phyloModel tree len gen
  let output = sequencesToFasta $ toSequenceList msa
  L.writeFile outFile output
  unless quiet $ do
    putStrLn ""
    putStrLn ("Output written to file '" ++ outFile ++ "'.")
