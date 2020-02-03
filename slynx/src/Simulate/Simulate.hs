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
  ( simulateCmd )
where

import           Control.Applicative                               ((<|>))
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad                                     (unless,
                                                                    when)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy                              as L
import qualified Data.ByteString.Lazy.Char8                        as LC
import           Data.Maybe
import qualified Data.Set                                          as Set
import qualified Data.Text                                         as T
import qualified Data.Text.Lazy                                    as LT
import qualified Data.Text.Lazy.Encoding                           as LT
import           Data.Tree
import qualified Data.Vector.Unboxed                               as V
import           Numeric.LinearAlgebra                             hiding ((<>))
import           System.Random.MWC

import           Simulate.Options
import           Simulate.PhyloModel

import           ELynx.Data.Alphabet.Alphabet                      as A
import           ELynx.Data.MarkovProcess.GammaRateHeterogeneity
import qualified ELynx.Data.MarkovProcess.MixtureModel             as M
import qualified ELynx.Data.MarkovProcess.PhyloModel               as P
import qualified ELynx.Data.MarkovProcess.SubstitutionModel        as SM
import qualified ELynx.Data.Sequence.Alignment                     as A
import qualified ELynx.Data.Sequence.Sequence                      as Seq hiding
                                                                           (name)
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.Tree
import           ELynx.Export.Sequence.Fasta
import           ELynx.Import.MarkovProcess.EDMModelPhylobayes     hiding
                                                                    (Parser)
import           ELynx.Import.MarkovProcess.SiteprofilesPhylobayes hiding
                                                                    (Parser)
import           ELynx.Import.Tree.Newick                          hiding (name)
import           ELynx.Simulate.MarkovProcessAlongTree

import           ELynx.Tools.ByteString
import           ELynx.Tools.Concurrent
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Misc

-- Simulate a 'Alignment' for a given phylogenetic model,
-- phylogenetic tree, and alignment length.
simulateAlignment :: (Measurable a, Named a)
            => P.PhyloModel -> Tree a -> Int -> GenIO
            -> IO A.Alignment
simulateAlignment pm t n g = do
  c  <- getNumCapabilities
  gs <- splitGen c g
  let chunks = getChunks c n
  leafStatesS <- case pm of
    -- TODO: This parallelization is not very intelligent, because the matrices
    -- exponentiation is done in all threads. So ten threads will exponentiate
    -- the same matrix ten times.
    P.SubstitutionModel sm -> mapConcurrently
      (\(num, gen) -> simulateAndFlatten num d e t gen) (zip chunks gs)
      where d = SM.stationaryDistribution sm
            e = SM.exchangeabilityMatrix sm
    -- P.MixtureModel mm      -> mapConcurrently
    --   (\(num, gen) -> simulateAndFlattenNSitesAlongTreeMixtureModel num ws ds es t gen) (zip chunks gs)
    P.MixtureModel mm      -> simulateAndFlattenMixtureModelPar n ws ds es t g
      where
        ws = vector $ M.getWeights mm
        ds = map SM.stationaryDistribution $ M.getSubstitutionModels mm
        es = map SM.exchangeabilityMatrix  $ M.getSubstitutionModels mm
  -- XXX: The horizontal concatenation might be slow. If so, 'concatenateSeqs'
  -- or 'concatenateAlignments' can be used, which directly appends vectors.
  let leafStates = horizontalConcat leafStatesS
      leafNames  = map getName $ leaves t
      code       = P.getAlphabet pm
      -- XXX: Probably use type safe stuff here?
      alph       = A.all $ alphabetSpec code
      sequences  = [ Seq.Sequence sName "" code (V.fromList $ map (`Set.elemAt` alph) ss) |
                    (sName, ss) <- zip leafNames leafStates ]
  return $ either error id $ A.fromSequences sequences

-- Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> L.ByteString
summarizeEDMComponents cs = LC.pack
                            $ "Empiricial distribution mixture model with "
                            ++ show (length cs) ++ " components."

-- XXX. Maybe provide human readable model file. But then, why is this
-- necessary. A human readable summary is reported anyways, and for Protein
-- models the exchangeabilities are too many.
reportModel :: Maybe FilePath -> P.PhyloModel -> Simulate ()
reportModel Nothing      _ = $(logInfo) "No output file provided; omit writing machine-readable phylogenetic model."
reportModel (Just outFn) m = do
  let modelFn = outFn <> ".model.gz"
  out "model definition (machine readable)" (bShow m <> "\n") (Just modelFn)

-- | Simulate sequences.
simulateCmd :: Maybe FilePath -> Simulate ()
simulateCmd outFn = do
  a <- lift ask
  let treeFile = argsTreeFile a

  $(logInfo) ""
  $(logInfo) $ T.pack $ "Read tree from file '" ++ treeFile ++ "'."
  tree <- liftIO $ parseFileWith newick treeFile
  $(logInfo) $ LT.toStrict $ LT.decodeUtf8 $ summarize tree

  let edmFile = argsEDMFile a
  let sProfileFiles = argsSiteprofilesFiles a
  $(logInfo) ""
  $(logDebug) "Read EDM file or siteprofile files."
  when (isJust edmFile && isJust sProfileFiles) $ error "Got both: --edm-file and --siteprofile-files."
  edmCs <- case edmFile of
    Nothing   -> return Nothing
    Just edmF -> do
      $(logInfo) "Read EDM file."
      liftIO $ Just <$> parseFileWith phylobayes edmF
  maybe (return ()) ($(logInfo) . LT.toStrict . LT.decodeUtf8 . summarizeEDMComponents) edmCs
  sProfiles <- case sProfileFiles of
    Nothing   -> return Nothing
    Just fns -> do
      $(logInfo) $ T.pack $ "Read siteprofiles from " ++ show (length fns) ++ " file(s)."
      $(logDebug) $ T.pack $ "The file names are:" ++ show fns
      xs <- liftIO $ mapM (parseFileWith siteprofiles) fns
      return $ Just $ concat xs
  maybe (return ()) ($(logInfo) . LT.toStrict . LT.decodeUtf8 . summarizeEDMComponents) sProfiles
  let edmCsOrSiteprofiles = edmCs <|> sProfiles

  $(logInfo) "Read model string."
  let ms = argsSubstitutionModelString a
      mm = argsMixtureModelString a
      mws = argsMixtureWeights a
      eitherPhyloModel' = getPhyloModel ms mm mws edmCsOrSiteprofiles
  phyloModel' <- case eitherPhyloModel' of
    Left err -> lift $ error err
    Right pm -> return pm

  let maybeGammaParams = argsGammaParams a
  phyloModel <- case maybeGammaParams of
    Nothing         -> do
      $(logInfo) $ LT.toStrict $ LT.decodeUtf8
        $ LC.unlines $ P.summarize phyloModel'
      return phyloModel'
    Just (n, alpha) -> do
      $(logInfo) $ LT.toStrict $ LT.decodeUtf8
        $ LC.intercalate "\n" $ P.summarize phyloModel'
      $(logInfo) ""
      $(logInfo) $ LT.toStrict $ LT.decodeUtf8
        $ LC.intercalate "\n" $ summarizeGammaRateHeterogeneity n alpha
      return $ expand n alpha phyloModel'

  -- -- XXX: Do not report possibly huge empirical distribution mixture models
  -- -- for now, because it takes too long and uses too much disk space :).
  unless (isJust sProfiles) (
    do $(logInfo) ""
       reportModel outFn phyloModel
       $(logInfo) ""
    )

  $(logInfo) "Simulate alignment."
  let alignmentLength = argsLength a
  $(logInfo) $ T.pack $ "Length: " <> show alignmentLength <> "."
  let maybeSeed = argsMaybeSeed a
  gen <- case maybeSeed of
    Nothing -> $(logInfo) "Seed: random."
               >> liftIO createSystemRandom
    Just s  -> $(logInfo) (T.pack ("Seed: " <> show s <> "."))
               >> liftIO (initialize (V.fromList s))
  alignment <- liftIO $ simulateAlignment phyloModel tree alignmentLength gen
  let output = (sequencesToFasta . A.toSequences) alignment
      outFile = (<> ".fasta") <$> outFn
  $(logInfo) ""
  out "simulated multi sequence alignment" output outFile
