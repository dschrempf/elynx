{-# LANGUAGE TemplateHaskell #-}
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
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import qualified Data.ByteString                                  as B
import qualified Data.ByteString.Lazy                             as L
import qualified Data.ByteString.Lazy.Char8                       as LC
import qualified Data.Text                                        as T
import           Data.Text.Encoding                               as E
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
summarizeEDMComponents :: [EDMComponent] -> T.Text
summarizeEDMComponents cs = T.pack
                            $ "Empiricial distribution mixture model with "
                            ++ show (length cs) ++ " components"

-- | The logging monad is wrapped around the actual state monad of the
-- simulator. However, this logging feature is not used at the moment (for
-- historical reasons).
type Simulation = LoggingT (StateT EvoModSimArgs IO)

getP :: LoggingT (StateT EvoModSimArgs IO) EvoModSimArgs
getP = lift get

logT :: T.Text -> LoggingT (StateT EvoModSimArgs IO) ()
logT = logInfoN

logS :: String -> LoggingT (StateT EvoModSimArgs IO) ()
logS = logInfoN . T.pack

logLBS :: LC.ByteString -> LoggingT (StateT EvoModSimArgs IO) ()
logLBS = logInfoN . E.decodeUtf8 . LC.toStrict

logSBS :: B.ByteString -> LoggingT (StateT EvoModSimArgs IO) ()
logSBS = logInfoN . E.decodeUtf8

simulate :: Simulation ()
simulate = do
  header <- liftIO programHeader
  logS header
  logS "Read tree."
  treeFile <- argsTreeFile <$> getP
  tree <- liftIO $ parseFileWith newick treeFile
  logLBS $ summarize tree
  maybeEDMFile <- argsMaybeEDMFile <$> getP
  edmCs <- case maybeEDMFile of
    Nothing   -> return Nothing
    Just edmF -> do
      logS "Read EDM file."
      liftIO $ Just <$> parseFileWith phylobayes edmF
  maybe (return ()) (logT . summarizeEDMComponents) edmCs
  logS "Read model string."
  phyloModelStr <- argsPhyloModelString <$> getP
  maybeWeights <- argsMaybeMixtureWeights <$> getP
  maybeGammaParams <- argsMaybeGammaParams <$> getP
  let phyloModel' = parseByteStringWith (phyloModelString edmCs maybeWeights) phyloModelStr
      phyloModel = case maybeGammaParams of
        Nothing         -> phyloModel'
        Just (n, alpha) -> expand n alpha phyloModel'
  alignmentLength <- argsLength <$> getP
  logLBS $ LC.unlines $ pmSummarize phyloModel
  logS "Simulate alignment."
  logS $ "Length: " ++ show alignmentLength ++ "."
  maybeSeed <- argsMaybeSeed <$> getP
  gen <- case maybeSeed of
    Nothing -> logS "Seed: random"
               >> liftIO createSystemRandom
    Just s  -> logS ("Seed: " ++ show s ++ ".")
               >> liftIO (initialize (V.fromList s))
  msa <- liftIO $ simulateMSA phyloModel tree alignmentLength gen
  let output = sequencesToFasta $ toSequenceList msa
  outFileAlignment <- argsFileOut <$> getP
  liftIO $ L.writeFile outFileAlignment output
  logS ("Output written to file '" ++ outFileAlignment ++ "'.")

main :: IO ()
main = do
  params <- parseEvoModSimArgs
  if argsQuiet params
    -- TODO: Let error messages through.
    -- TODO: But do not through errors in library functions.
    then evalStateT (runStdoutLoggingT $ filterLogger (\_ _ -> True) simulate) params
    else evalStateT (runStdoutLoggingT simulate) params
