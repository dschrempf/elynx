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
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.ByteString.Lazy                             as L
import qualified Data.ByteString.Lazy.Char8                       as LC
import           Data.Tree
import           System.IO

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

-- Simulate a 'MultiSequenceAlignment' for a given phylogenetic model,
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

-- Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> L.ByteString
summarizeEDMComponents cs = LC.pack
                            $ "Empiricial distribution mixture model with "
                            ++ show (length cs) ++ " components"

type Simulation = StateT Params IO

data Params = Params { arguments :: EvoModSimArgs
                     , logHandle :: Handle }

logS :: String -> Simulation ()
logS msg = do
  q <- argsQuiet . arguments <$> get
  h <- logHandle <$> get
  unless q $ lift $ putStrLn msg
  lift $ hPutStrLn h msg

logLBS :: LC.ByteString -> Simulation ()
logLBS = logS . LC.unpack

simulate :: Simulation ()
simulate = do
  args <- arguments <$> get
  header <- lift programHeader
  logS header
  logS "Read tree."
  let treeFile = argsTreeFile args
  tree <- lift $ parseFileWith newick treeFile
  logLBS $ summarize tree
  let maybeEDMFile = argsMaybeEDMFile args
  edmCs <- case maybeEDMFile of
    Nothing   -> return Nothing
    Just edmF -> do
      logS "Read EDM file."
      lift $ Just <$> parseFileWith phylobayes edmF
  maybe (return ()) (logLBS . summarizeEDMComponents) edmCs
  logS "Read model string."
  let phyloModelStr = argsPhyloModelString args
      maybeWeights = argsMaybeMixtureWeights args
      maybeGammaParams = argsMaybeGammaParams args
      phyloModel' = parseByteStringWith (phyloModelString edmCs maybeWeights) phyloModelStr
      phyloModel = case maybeGammaParams of
        Nothing         -> phyloModel'
        Just (n, alpha) -> expand n alpha phyloModel'
      alignmentLength = argsLength args
  -- TODO: Summarize model before it is expanded and state that gamma rate heterogeneity is used.
  -- TODO: Rigorous logging.
  logLBS $ LC.unlines $ pmSummarize phyloModel
  logS "Simulate alignment."
  logS $ "Length: " ++ show alignmentLength ++ "."
  let maybeSeed = argsMaybeSeed args
  gen <- case maybeSeed of
    Nothing -> logS "Seed: random"
               >> lift createSystemRandom
    Just s  -> logS ("Seed: " ++ show s ++ ".")
               >> lift (initialize (V.fromList s))
  msa <- lift $ simulateMSA phyloModel tree alignmentLength gen
  let output = sequencesToFasta $ toSequenceList msa
      outFile = argsFileOut args
  lift $ L.writeFile outFile output
  logS ("Output written to file '" ++ outFile ++ "'.")

main :: IO ()
main = do
  args <- parseEvoModSimArgs

  let logFile = argsFileOut args ++ ".log"
  logH <- openFile logFile WriteMode
  let params = Params args logH

  evalStateT simulate params

  hClose logH
