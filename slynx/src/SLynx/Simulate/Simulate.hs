{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  SLynx.Simulate.Simulate
-- Description :  Simulate multiple sequence alignments
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Jan 28 14:12:52 2019.
module SLynx.Simulate.Simulate
  ( simulateCmd,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.NonEmpty (toList)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vector.Unboxed as V
import ELynx.Data.Alphabet.Alphabet as A
import ELynx.Data.MarkovProcess.GammaRateHeterogeneity
import qualified ELynx.Data.MarkovProcess.MixtureModel as M
import qualified ELynx.Data.MarkovProcess.PhyloModel as P
import qualified ELynx.Data.MarkovProcess.SubstitutionModel as SM
import qualified ELynx.Data.Sequence.Alignment as A
import qualified ELynx.Data.Sequence.Sequence as Seq hiding
  ( name,
  )
import ELynx.Export.Sequence.Fasta
import ELynx.Import.MarkovProcess.EDMModelPhylobayes
import ELynx.Import.MarkovProcess.SiteprofilesPhylobayes
import ELynx.Simulate.MarkovProcessAlongTree
import ELynx.Tools
import ELynx.Tree
import Numeric.LinearAlgebra hiding
  ( toList,
    (<>),
  )
import SLynx.Simulate.Options
import SLynx.Simulate.PhyloModel
import System.Random.MWC
import Text.Printf

-- Simulate a 'Alignment' for a given phylogenetic model,
-- phylogenetic tree, and alignment length.
simulateAlignment ::
  (Measurable e, Named a) =>
  P.PhyloModel ->
  Tree e a ->
  Int ->
  GenIO ->
  IO A.Alignment
simulateAlignment pm t' n g = do
  let t = fromLength . getLen <$> toTreeBranchLabels t'
  c <- getNumCapabilities
  gs <- splitGen c g
  let chunks = getChunks c n
  leafStatesS <- case pm of
    -- TODO @performace: This parallelization is not very intelligent, because
    -- the matrix exponentiation is done in all threads. So ten threads will
    -- exponentiate the same matrix ten times.
    P.SubstitutionModel sm ->
      mapConcurrently
        (\(num, gen) -> simulateAndFlatten num d e t gen)
        (zip chunks gs)
      where
        d = SM.stationaryDistribution sm
        e = SM.exchangeabilityMatrix sm
    -- P.MixtureModel mm      -> mapConcurrently
    --   (\(num, gen) -> simulateAndFlattenNSitesAlongTreeMixtureModel num ws ds es t gen) (zip chunks gs)
    P.MixtureModel mm -> simulateAndFlattenMixtureModelPar n ws ds es t g
      where
        ws = vector . toList $ M.getWeights mm
        ds = map SM.stationaryDistribution $ toList $ M.getSubstitutionModels mm
        es = map SM.exchangeabilityMatrix $ toList $ M.getSubstitutionModels mm
  -- XXX @performace. The horizontal concatenation might be slow. If so,
  -- 'concatenateSeqs' or 'concatenateAlignments' can be used, which directly
  -- appends vectors.
  let leafStates = horizontalConcat leafStatesS
      leafNames = map getName $ leaves t'
      code = P.getAlphabet pm
      -- XXX: Probably use type safe stuff here?
      alph = A.all $ alphabetSpec code
      sequences =
        [ Seq.Sequence (fromName sName) "" code (V.fromList $ map (`Set.elemAt` alph) ss)
          | (sName, ss) <- zip leafNames leafStates
        ]
  return $ either error id $ A.fromSequences sequences

-- Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> BL.ByteString
summarizeEDMComponents cs =
  BL.pack $
    "Empiricial distribution mixture model with "
      ++ show (length cs)
      ++ " components."

reportModel :: P.PhyloModel -> ELynx SimulateArguments ()
reportModel m = do
  as <- global <$> ask
  if writeElynxFile as
    then (do let bn = outFileBaseName as
             case bn of
               Nothing ->
                 $(logInfo)
                   "No output file provided; omit writing machine-readable phylogenetic model."
               Just _ ->
                 out "model definition (machine readable)" (BL.pack (show m) <> "\n") ".model.gz")
    else $(logInfo) "No elynx file required; omit writing machine-readable phylogenetic model."

pretty :: Length -> String
pretty = printf "%.5f" . fromLength

prettyRow :: String -> String -> BL.ByteString
prettyRow name val = alignLeft 33 n <> alignRight 8 v
  where
    n = BL.pack name
    v = BL.pack val

-- | Examine branches of a tree.
summarizeLengths :: Measurable e => Tree e a -> BL.ByteString
summarizeLengths t =
  BL.intercalate
    "\n"
    [ prettyRow "Origin height: " $ pretty h,
      prettyRow "Average distance origin to leaves: " $ pretty h',
      prettyRow "Total branch length: " $ pretty b
    ]
  where
    n = length $ leaves t
    h = height t
    h' = sum (distancesOriginLeaves t) / fromIntegral n
    b = totalLength t

-- Summarize a substitution model; lines to be printed to screen or log.
summarizeSM :: SM.SubstitutionModel -> [BL.ByteString]
summarizeSM sm =
  map BL.pack $
    (show (SM.alphabet sm) ++ " substitution model: " ++ SM.name sm ++ ".") :
    ["Parameters: " ++ show (SM.params sm) ++ "." | not (null (SM.params sm))]
      ++ case SM.alphabet sm of
        DNA ->
          [ "Stationary distribution: "
              ++ dispv precision (SM.stationaryDistribution sm)
              ++ ".",
            "Exchangeability matrix:\n"
              ++ dispmi 2 precision (SM.exchangeabilityMatrix sm)
              ++ ".",
            "Scale: " ++ show (roundN precision $ SM.totalRate sm) ++ "."
          ]
        Protein ->
          [ "Stationary distribution: "
              ++ dispv precision (SM.stationaryDistribution sm)
              ++ ".",
            "Scale: " ++ show (roundN precision $ SM.totalRate sm) ++ "."
          ]
        _ ->
          error
            "Extended character sets are not supported with substitution models."

-- Summarize a mixture model component; lines to be printed to screen or log.
summarizeMMComponent :: M.Component -> [BL.ByteString]
summarizeMMComponent c =
  BL.pack "Weight: "
    <> (BB.toLazyByteString . BB.doubleDec $ M.weight c) :
  summarizeSM (M.substModel c)

-- Summarize a mixture model; lines to be printed to screen or log.
summarizeMM :: M.MixtureModel -> [BL.ByteString]
summarizeMM m =
  [ BL.pack $ "Mixture model: " ++ M.name m ++ ".",
    BL.pack $ "Number of components: " ++ show n ++ "."
  ]
    ++ detail
  where
    n = length $ M.components m
    detail =
      if n <= 100
        then
          concat
            [ BL.pack ("Component " ++ show i ++ ":") : summarizeMMComponent c
              | (i, c) <- zip [1 :: Int ..] (toList $ M.components m)
            ]
        else []

-- Summarize a phylogenetic model; lines to be printed to screen or log.
summarizePM :: P.PhyloModel -> [BL.ByteString]
summarizePM (P.MixtureModel mm) = summarizeMM mm
summarizePM (P.SubstitutionModel sm) = summarizeSM sm

-- | Simulate sequences.
simulateCmd :: ELynx SimulateArguments ()
simulateCmd = do
  l <- local <$> ask
  let treeFile = argsTreeFile l
  $(logInfo) ""
  $(logInfo) $ T.pack $ "Read tree from file '" ++ treeFile ++ "'."
  tree <- liftIO $ parseFileWith (newick Standard) treeFile
  let t' = either error id $ phyloToLengthTree tree
  $(logInfo) $ T.pack $ "Number of leaves: " ++ show (length $ leaves t')
  $(logInfo) $ LT.toStrict $ LT.decodeUtf8 $ summarizeLengths t'
  let edmFile = argsEDMFile l
  let sProfileFiles = argsSiteprofilesFiles l
  $(logInfo) ""
  $(logDebug) "Read EDM file or siteprofile files."
  when (isJust edmFile && isJust sProfileFiles) $
    error "Got both: --edm-file and --siteprofile-files."
  edmCs <- case edmFile of
    Nothing -> return Nothing
    Just edmF -> do
      $(logInfo) "Read EDM file."
      liftIO $ Just <$> parseFileWith phylobayes edmF
  maybe
    (return ())
    ($(logInfo) . LT.toStrict . LT.decodeUtf8 . summarizeEDMComponents)
    edmCs
  sProfiles <- case sProfileFiles of
    Nothing -> return Nothing
    Just fns -> do
      $(logInfo) $
        T.pack $
          "Read siteprofiles from "
            ++ show (length fns)
            ++ " file(s)."
      $(logDebug) $ T.pack $ "The file names are:" ++ show fns
      xs <- liftIO $ mapM (parseFileWith siteprofiles) fns
      return $ Just $ concat xs
  maybe
    (return ())
    ($(logInfo) . LT.toStrict . LT.decodeUtf8 . summarizeEDMComponents)
    sProfiles
  let edmCsOrSiteprofiles = edmCs <|> sProfiles
  $(logInfo) "Read model string."
  let ms = argsSubstitutionModelString l
      mm = argsMixtureModelString l
      mws = argsMixtureWeights l
      eitherPhyloModel' = getPhyloModel ms mm mws edmCsOrSiteprofiles
  phyloModel' <- case eitherPhyloModel' of
    Left err -> lift $ error err
    Right pm -> return pm
  let maybeGammaParams = argsGammaParams l
  phyloModel <- case maybeGammaParams of
    Nothing -> do
      $(logInfo) $
        LT.toStrict $
          LT.decodeUtf8 $
            BL.unlines $
              summarizePM
                phyloModel'
      return phyloModel'
    Just (n, alpha) -> do
      $(logInfo) $
        LT.toStrict $
          LT.decodeUtf8 $
            BL.intercalate "\n" $
              summarizePM phyloModel'
      $(logInfo) ""
      $(logInfo) $
        LT.toStrict $
          LT.decodeUtf8 $
            BL.intercalate "\n" $
              summarizeGammaRateHeterogeneity n alpha
      return $ expand n alpha phyloModel'
  reportModel phyloModel
  $(logInfo) "Simulate alignment."
  let alignmentLength = argsLength l
  $(logInfo) $ T.pack $ "Length: " <> show alignmentLength <> "."
  gen <- case argsSeed l of
    Random ->
      error "simulateCmd: seed not available; please contact maintainer."
    Fixed s -> liftIO $ initialize s
  alignment <- liftIO $ simulateAlignment phyloModel t' alignmentLength gen
  let output = (sequencesToFasta . A.toSequences) alignment
  $(logInfo) ""
  out "simulated multi sequence alignment" output ".fasta"
