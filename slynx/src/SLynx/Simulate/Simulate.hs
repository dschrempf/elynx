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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import ELynx.Data.Alphabet.Alphabet as A
import qualified ELynx.Data.MarkovProcess.AminoAcid as MA
import ELynx.Data.MarkovProcess.GammaRateHeterogeneity
import qualified ELynx.Data.MarkovProcess.MixtureModel as MM
import qualified ELynx.Data.MarkovProcess.PhyloModel as MP
import qualified ELynx.Data.MarkovProcess.RateMatrix as MR
import qualified ELynx.Data.MarkovProcess.SubstitutionModel as MS
import qualified ELynx.Data.Sequence.Sequence as Seq hiding
  ( name,
  )
import ELynx.Export.Sequence.Fasta
import ELynx.Import.MarkovProcess.EDMModelPhylobayes
import ELynx.Import.MarkovProcess.SiteprofilesPhylobayes
import ELynx.Simulate.MarkovProcessAlongTree
import ELynx.Tools
import ELynx.Tree
import SLynx.Simulate.Options
import SLynx.Simulate.PhyloModel
import System.Random.MWC
import Text.Printf

getDistLine :: Int -> MR.StationaryDistribution -> BB.Builder
getDistLine i d =
  BB.intDec i
    <> BB.char8 ' '
    <> s
  where
    s = mconcat $ intersperse (BB.char8 ' ') $ map BB.doubleDec $ VS.toList d

writeSiteDists :: [Int] -> V.Vector MR.StationaryDistribution -> ELynx SimulateArguments ()
-- writeSiteDists is ds = out "site distributions of distribution mixture model" output ".sitedists"
writeSiteDists componentIs ds = do
  mbn <- outFileBaseName . global <$> ask
  case mbn of
    Nothing -> return ()
    Just bn -> liftIO $ BL.writeFile (bn <> ".sitedists") output
  where
    dsPaml = V.map MA.alphaToPamlVec ds
    lns = [getDistLine i d | (i, c) <- zip [1 ..] componentIs, let d = dsPaml V.! c]
    output = BB.toLazyByteString $ mconcat $ intersperse (BB.char8 '\n') lns

-- Simulate a 'Alignment' for a given phylogenetic model,
-- phylogenetic tree, and alignment length.
simulateAlignment ::
  (HasLength e, HasName a) =>
  MP.PhyloModel ->
  Tree e a ->
  Int ->
  GenIO ->
  ELynx SimulateArguments ()
simulateAlignment pm t' n g = do
  let t = fromLength . getLen <$> toTreeBranchLabels t'
  leafStates <- case pm of
    MP.SubstitutionModel sm -> liftIO $ simulateAndFlattenPar n d e t g
      where
        d = MS.stationaryDistribution sm
        e = MS.exchangeabilityMatrix sm
    MP.MixtureModel mm -> do
      (cs, ss) <- liftIO $ simulateAndFlattenMixtureModelPar n ws ds es t g
      -- TODO: Writing site distributions only makes sense for EDM models.
      -- Remove this if not needed or improve to be helpful in general.
      writeSiteDists cs ds
      return ss
      where
        ws = MM.getWeights mm
        ds = V.map MS.stationaryDistribution $ MM.getSubstitutionModels mm
        es = V.map MS.exchangeabilityMatrix $ MM.getSubstitutionModels mm
  let leafNames = map getName $ leaves t'
      code = MP.getAlphabet pm
      -- XXX: Probably use type safe stuff here?
      alph = A.all $ alphabetSpec code
      sequences =
        [ Seq.Sequence (fromName sName) "" code (U.fromList $ map (`Set.elemAt` alph) ss)
          | (sName, ss) <- zip leafNames leafStates
        ]
      output = sequencesToFasta sequences
  $(logInfo) ""
  out "simulated multi sequence alignment" output ".fasta"

-- Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> BL.ByteString
summarizeEDMComponents cs =
  BL.pack $
    "Empiricial distribution mixture model with "
      ++ show (length cs)
      ++ " components."

reportModel :: MP.PhyloModel -> ELynx SimulateArguments ()
reportModel m = do
  as <- global <$> ask
  if writeElynxFile as
    then
      ( do
          let bn = outFileBaseName as
          case bn of
            Nothing ->
              $(logInfo)
                "No output file provided; omit writing machine-readable phylogenetic model."
            Just _ ->
              out "model definition (machine readable)" (BL.pack (show m) <> "\n") ".model.gz"
      )
    else $(logInfo) "No elynx file required; omit writing machine-readable phylogenetic model."

pretty :: Length -> String
pretty = printf "%.5f" . fromLength

prettyRow :: String -> String -> BL.ByteString
prettyRow name val = alignLeft 33 n <> alignRight 8 v
  where
    n = BL.pack name
    v = BL.pack val

-- | Examine branches of a tree.
summarizeLengths :: HasLength e => Tree e a -> BL.ByteString
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
    b = totalBranchLength t

-- Summarize a substitution model; lines to be printed to screen or log.
summarizeSM :: MS.SubstitutionModel -> [BL.ByteString]
summarizeSM sm =
  map BL.pack $
    (show (MS.alphabet sm) ++ " substitution model: " ++ MS.name sm ++ ".") :
    ["Parameters: " ++ show (MS.params sm) ++ "." | not (null (MS.params sm))]
      ++ case MS.alphabet sm of
        DNA ->
          [ "Stationary distribution: "
              ++ dispv precision (MS.stationaryDistribution sm)
              ++ ".",
            "Exchangeability matrix:\n"
              ++ dispmi 2 precision (MS.exchangeabilityMatrix sm)
              ++ ".",
            "Scale: " ++ show (roundN precision $ MS.totalRate sm) ++ "."
          ]
        Protein ->
          [ "Stationary distribution: "
              ++ dispv precision (MS.stationaryDistribution sm)
              ++ ".",
            "Scale: " ++ show (roundN precision $ MS.totalRate sm) ++ "."
          ]
        _ ->
          error
            "Extended character sets are not supported with substitution models."

-- Summarize a mixture model component; lines to be printed to screen or log.
summarizeMMComponent :: MM.Component -> [BL.ByteString]
summarizeMMComponent c =
  BL.pack "Weight: "
    <> (BB.toLazyByteString . BB.doubleDec $ MM.weight c) :
  summarizeSM (MM.substModel c)

-- Summarize a mixture model; lines to be printed to screen or log.
summarizeMM :: MM.MixtureModel -> [BL.ByteString]
summarizeMM m =
  [ BL.pack $ "Mixture model: " ++ MM.name m ++ ".",
    BL.pack $ "Number of components: " ++ show n ++ "."
  ]
    ++ detail
  where
    n = length $ MM.components m
    detail =
      if n <= 100
        then
          concat
            [ BL.pack ("Component " ++ show i ++ ":") : summarizeMMComponent c
              | (i, c) <- zip [1 :: Int ..] (V.toList $ MM.components m)
            ]
        else []

-- Summarize a phylogenetic model; lines to be printed to screen or log.
summarizePM :: MP.PhyloModel -> [BL.ByteString]
summarizePM (MP.MixtureModel mm) = summarizeMM mm
summarizePM (MP.SubstitutionModel sm) = summarizeSM sm

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
    Random -> error "simulateCmd: seed not available; please contact maintainer."
    Fixed s -> liftIO $ initialize s
  simulateAlignment phyloModel t' alignmentLength gen
