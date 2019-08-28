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
import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy                             as L
import qualified Data.ByteString.Lazy.Char8                       as LC
import qualified Data.Set                                         as Set
import           Data.Tree
import qualified Data.Vector.Unboxed                              as V
import           Numeric.LinearAlgebra
import           System.IO
import           System.Random.MWC

import           OptionsSeqSim
import           ParsePhyloModel

import           EvoMod.Data.Alphabet.Alphabet                    as A
import           EvoMod.Data.MarkovProcess.GammaRateHeterogeneity
import qualified EvoMod.Data.MarkovProcess.MixtureModel           as M
import qualified EvoMod.Data.MarkovProcess.PhyloModel             as P
import qualified EvoMod.Data.MarkovProcess.SubstitutionModel      as S
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence                    hiding (name)
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.NamedTree
import           EvoMod.Data.Tree.Tree
import           EvoMod.Export.Sequence.Fasta
import           EvoMod.Import.MarkovProcess.EDMModelPhylobayes   hiding
                                                                   (Parser)
import           EvoMod.Import.Tree.Newick                        hiding (name)
import           EvoMod.Simulate.MarkovProcessAlongTree
import           EvoMod.Tools.Concurrent
import           EvoMod.Tools.InputOutput
import           EvoMod.Tools.Logger
import           EvoMod.Tools.Misc
import           EvoMod.Tools.Options

-- Simulate a 'MultiSequenceAlignment' for a given phylogenetic model,
-- phylogenetic tree, and alignment length.
simulateMSA :: (Measurable a, Named a)
            => P.PhyloModel -> Tree a -> Int -> GenIO
            -> IO MultiSequenceAlignment
simulateMSA pm t n g = do
  c  <- getNumCapabilities
  gs <- splitGen c g
  let chunks = getChunks c n
  leafStatesS <- case pm of
    P.SubstitutionModel sm -> mapConcurrently
      (\(num, gen) -> simulateAndFlattenNSitesAlongTree num d e t gen) (zip chunks gs)
      where d = sm ^. S.stationaryDistribution
            e = sm ^. S.exchangeabilityMatrix
    P.MixtureModel mm      -> mapConcurrently
      (\(num, gen) -> simulateAndFlattenNSitesAlongTreeMixtureModel num ws ds es t gen) (zip chunks gs)
      where
        ws = vector $ M.getWeights mm
        ds = map (view S.stationaryDistribution) $ M.getSubstitutionModels mm
        es = map (view S.exchangeabilityMatrix) $ M.getSubstitutionModels mm
  -- XXX: The horizontal concatenation might be slow. If so, 'concatenateSeqs'
  -- or 'concatenateMSAs' can be used, which directly appends vectors.
  let leafStates = horizontalConcat leafStatesS
      leafNames  = map name $ leaves t
      code       = P.getAlphabet pm
      -- XXX: Probably use type safe stuff here?
      alph       = A.all $ alphabetSpec code
      sequences  = [ Sequence sName code (V.fromList $ map (`Set.elemAt` alph) ss) |
                    (sName, ss) <- zip leafNames leafStates ]
  return $ either error id $ fromSequenceList sequences

-- Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> L.ByteString
summarizeEDMComponents cs = LC.pack
                            $ "Empiricial distribution mixture model with "
                            ++ show (length cs) ++ " components."

type Simulation = ReaderT Params IO

data Params = Params { arguments  :: Args
                     , mLogHandle :: Maybe Handle }

instance Logger Params where
  verbosity = argsVerbosity . arguments
  mHandle   = mLogHandle

reportModel :: P.PhyloModel -> Simulation ()
reportModel m = do
  args <- arguments <$> ask
  let fnOut = argsOutFileBaseName args
      modelFn = fnOut ++ ".model"
  -- TODO. Provide human readable model file.
  lift $ writeFile modelFn (show m)
  logS $ "Exact model definition written to '" ++ modelFn ++ "' (machine readable)."
  logS ""

simulate :: Simulation ()
simulate = do
  args <- arguments <$> ask
  lift programHeader >>= logS
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
  let ms = argsMaybeSubstitutionModelString args
      mm = argsMaybeMixtureModelString args
      mws = argsMaybeMixtureWeights args
      eitherPhyloModel' = getPhyloModel ms mm mws edmCs
  phyloModel' <- case eitherPhyloModel' of
    Left err -> lift $ error err
    Right pm -> return pm

  let maybeGammaParams = argsMaybeGammaParams args
  phyloModel <- case maybeGammaParams of
    Nothing         -> do
      logLBS $ LC.unlines $ P.summarize phyloModel'
      return phyloModel'
    Just (n, alpha) -> do
      logLBS $ LC.unlines $ P.summarize phyloModel' ++ summarizeGammaRateHeterogeneity n alpha
      return $ expand n alpha phyloModel'
  reportModel phyloModel

  logS "Simulate alignment."
  let alignmentLength = argsLength args
  logS $ "Length: " ++ show alignmentLength ++ "."
  let maybeSeed = argsMaybeSeed args
  gen <- case maybeSeed of
    Nothing -> logS "Seed: random"
               >> lift createSystemRandom
    Just s  -> logS ("Seed: " ++ show s ++ ".")
               >> lift (initialize (V.fromList s))
  msa <- lift $ simulateMSA phyloModel tree alignmentLength gen
  let output = (sequencesToFasta . toSequenceList) msa
      outFile = argsOutFileBaseName args ++ ".fasta"
  lift $ L.writeFile outFile output
  logS ("Output written to file '" ++ outFile ++ "'.")

main :: IO ()
main = do
  a <- parseArgs
  h <- setupLogger (Just $ argsOutFileBaseName a)
  runReaderT simulate (Params a h)
  closeLogger h
