{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Simulate.Simulate
Description :  Simulate multiple sequence alignments
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon Jan 28 14:12:52 2019.

-}

module Simulate.Simulate
  ( simulate )
where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy                            as L
import qualified Data.ByteString.Lazy.Char8                      as LC
import qualified Data.Set                                        as Set
import           Data.Tree
import qualified Data.Vector.Unboxed                             as V
import           Numeric.LinearAlgebra                           hiding ((<>))
import           System.Random.MWC

import           Simulate.Options
import           Simulate.PhyloModel

import           ELynx.Data.Alphabet.Alphabet                    as A
import           ELynx.Data.MarkovProcess.GammaRateHeterogeneity
import qualified ELynx.Data.MarkovProcess.MixtureModel           as M
import qualified ELynx.Data.MarkovProcess.PhyloModel             as P
import qualified ELynx.Data.MarkovProcess.SubstitutionModel      as S
import           ELynx.Data.Sequence.MultiSequenceAlignment
import           ELynx.Data.Sequence.Sequence                    hiding (name)
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.Tree
import           ELynx.Export.Sequence.Fasta
import           ELynx.Import.MarkovProcess.EDMModelPhylobayes   hiding (Parser)
import           ELynx.Import.Tree.Newick                        hiding (name)
import           ELynx.Simulate.MarkovProcessAlongTree

import           ELynx.Tools.ByteString
import           ELynx.Tools.Concurrent
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Misc
import           ELynx.Tools.Options

type Simulation = LoggingT (ReaderT Arguments IO)

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
      leafNames  = map getName $ leaves t
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

reportModel :: P.PhyloModel -> Simulation ()
reportModel m = do
  g <- globalArgs <$> lift ask
  let modelFn = (<> ".model") <$> outFileBaseName g
  -- TODO. Provide human readable model file.
  io "model definition (machine readable)" (bsShow m) modelFn

simulate :: Simulation ()
simulate = do
  h <- liftIO $ programHeader "seq-sim: Simulate sequences."
  $(logInfoSH) h
  Arguments g c <- lift ask
  $(logInfo) "Read tree."
  let treeFile = inFile g
  tree <- liftIO $ parseFileOrIOWith newick treeFile
  $(logInfoSH) $ summarize tree

  let edmFile = argsEDMFile c
  edmCs <- case edmFile of
    Nothing   -> return Nothing
    Just edmF -> do
      $(logInfo) "Read EDM file."
      liftIO $ Just <$> parseFileWith phylobayes edmF
  maybe (return ()) ($(logInfoSH) . summarizeEDMComponents) edmCs

  $(logInfo) "Read model string."
  let ms = argsSubstitutionModelString c
      mm = argsMixtureModelString c
      mws = argsMixtureWeights c
      eitherPhyloModel' = getPhyloModel ms mm mws edmCs
  phyloModel' <- case eitherPhyloModel' of
    Left err -> lift $ error err
    Right pm -> return pm

  let maybeGammaParams = argsGammaParams c
  phyloModel <- case maybeGammaParams of
    Nothing         -> do
      $(logInfoSH) $ LC.unlines $ P.summarize phyloModel'
      return phyloModel'
    Just (n, alpha) -> do
      $(logInfoSH) $ LC.unlines $ P.summarize phyloModel' ++ summarizeGammaRateHeterogeneity n alpha
      return $ expand n alpha phyloModel'
  reportModel phyloModel

  $(logInfo) "Simulate alignment."
  let alignmentLength = argsLength c
  $(logInfoSH) $ "Length: " <> show alignmentLength <> "."
  let maybeSeed = argsMaybeSeed c
  gen <- case maybeSeed of
    Nothing -> $(logInfo) "Seed: random"
               >> liftIO createSystemRandom
    Just s  -> $(logInfoSH) ("Seed: " <> bsShow s <> ".")
               >> liftIO (initialize (V.fromList s))
  msa <- liftIO $ simulateMSA phyloModel tree alignmentLength gen
  let output = (sequencesToFasta . toSequenceList) msa
      outFile = (<> ".fasta") <$> outFileBaseName g
  io "simulated multi sequence alignment" output outFile
