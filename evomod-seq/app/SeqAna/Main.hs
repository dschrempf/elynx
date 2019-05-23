{- |
Module      :  Main
Description :  Parse sequence file formats and analyze them
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 08:41:05 2018.

XXX: Provide possibility to parse and handle sequences with different codes.

-}

module Main where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8                  as L
import           Data.Maybe                                  (fromMaybe)
import qualified Data.Vector                                 as V
import           System.IO
import           System.Random.MWC
import           Text.Printf

import           OptionsSeqAna

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Data.Sequence.Translate
import           EvoMod.Export.Sequence.Fasta
import           EvoMod.Import.Sequence.Fasta
import           EvoMod.Tools.ByteString
import           EvoMod.Tools.InputOutput
import           EvoMod.Tools.Logger
import           EvoMod.Tools.Misc
import           EvoMod.Tools.Options

data Params = Params { arguments  :: Args
                     , mLogHandle :: Maybe Handle }

instance Logger Params where
  verbosity = argsVerbosity . arguments
  mHandle   = mLogHandle

type Seq = ReaderT Params IO

examineMSA :: Bool -> MultiSequenceAlignment -> L.ByteString
examineMSA perSiteFlag msa =
  L.unlines [ L.pack $ "Total number of columns in alignment: "
              ++ show (msaLength msa)
            , L.pack $ "Number of columns without gaps: "
              ++ show (msaLength msaNoGaps)
            , L.pack $ "Number of columns with standard characters only: "
              ++ show (msaLength msaOnlyStd)
            , L.empty
            , L.pack $ "Total number of characters: " ++ show nTot
            , L.pack $ "Standard (i.e., not extended IUPAC) characters: "
              ++ show (nTot - nIUPAC - nGaps - nUnknowns)
            , L.pack $ "Extended IUPAC characters: " ++ show nIUPAC
            , L.pack $ "Gaps: " ++ show nGaps
            , L.pack $ "Unknowns: " ++ show nUnknowns
            , L.pack $ "Percentage of standard characters: "
              ++ printf "%.3f" (1.0 - percentageIUPAC - percentageGaps - percentageUnknowns)
            , L.pack $ "Percentage of extended IUPAC characters: "
              ++ printf "%.3f" percentageIUPAC
            , L.pack $ "Percentage of gaps: "
              ++ printf "%.3f" percentageGaps
            , L.pack $ "Percentage of unknowns: "
              ++ printf "%.3f" percentageUnknowns
            , L.empty
            , L.pack "Mean effective number of used states:"
            , L.pack "Across whole alignment: "
              <> L.pack (printf "%.3f" kEffMean)
            , L.pack "Across columns without gaps: "
              <> L.pack (printf "%.3f" kEffMeanNoGaps)
            , L.pack "Across columns without extended IUPAC characters: "
              <> L.pack (printf "%.3f" kEffMeanOnlyStd)
            ]
  <> perSiteBS
  where
    nTot                = msaLength msa * msaNSequences msa
    nIUPAC              = countIUPACChars msa
    nGaps               = countGaps msa
    nUnknowns           = countUnknowns msa
    percentageIUPAC     = fromIntegral nIUPAC    / fromIntegral nTot :: Double
    percentageGaps      = fromIntegral nGaps     / fromIntegral nTot :: Double
    percentageUnknowns  = fromIntegral nUnknowns / fromIntegral nTot :: Double
    msaNoGaps           = filterColumnsNoGaps msa
    msaOnlyStd          = filterColumnsOnlyStd msaNoGaps
    kEffs               = kEff . toFrequencyData $ msa
    kEffsNoGaps         = kEff . toFrequencyData $ msaNoGaps
    kEffsOnlyStd        = kEff . toFrequencyData $ msaOnlyStd
    kEffMean            = sum kEffs / fromIntegral (length kEffs)
    kEffMeanNoGaps      = sum kEffsNoGaps  / fromIntegral (length kEffsNoGaps)
    kEffMeanOnlyStd     = sum kEffsOnlyStd / fromIntegral (length kEffsOnlyStd)
    perSiteBS           = if perSiteFlag
                          then L.unlines [ L.pack "Effective number of used states per site:"
                                         , L.pack . show $ kEffs
                                         ]
                          else L.empty

examineOneS :: Bool -> [Sequence] -> L.ByteString
examineOneS perSiteFlag ss = summarizeSequenceList ss <> sumMSA
  where sumMSA = if equalLength ss
                 then L.pack "\n" <> examineMSA perSiteFlag (fromSequenceList ss)
                 else L.empty

examineS :: Seq ()
examineS = do
  args <- arguments <$> ask
  let (Examine perSiteFlag) = argsCommand args
  sss <- readSeqss
  io $ L.intercalate (L.pack "\n") $
    map (examineOneS perSiteFlag) sss

concatenateS :: Seq ()
concatenateS = do
  sss <- readSeqss
  io $ sequencesToFasta $ concatenateSeqs sss

filterRowsS :: Seq ()
filterRowsS = do
  args <- arguments <$> ask
  let (FilterRows ml ms) = argsCommand args
  sss <- readSeqss
  let filters = map (fromMaybe id) [ filterLongerThan <$> ml
                                   , filterShorterThan <$> ms ]
  io $ sequencesToFasta $ compose filters $ concat sss

filterColumnsS :: Seq ()
filterColumnsS = do
  args <- arguments <$> ask
  let (FilterColumns ms) = argsCommand args
  sss <- readSeqss
  let msas = map fromSequenceList sss
  let filters = map (fromMaybe id) [ filterColumnsStd <$> ms ]
  -- TODO: ONE INPUT FILE, NOT MANY. ONLY CONCAT HAS MANY INPUT FILES.
  io $ L.concat $ map (sequencesToFasta . toSequenceList) [ compose filters msa | msa <- msas ]

-- subsample nSites nSamples msa gen
subsample :: (PrimMonad m)
          => Int -> Int -> MultiSequenceAlignment -> Gen (PrimState m) -> m [MultiSequenceAlignment]
subsample n m msa g = replicateM m $ randomSubSample n msa g

subSampleS :: Seq ()
subSampleS = do
  args <- arguments <$> ask
  let (SubSample n m ms) = argsCommand args
  sss <- readSeqss
  when (length sss > 1) $ error "can only sub-sample from one input file"
  g <- lift $ maybe createSystemRandom (initialize . V.fromList) ms
  let msa = fromSequenceList $ head sss
  samples <- subsample n m msa g
  let files = map (sequencesToFasta . toSequenceList) samples
  mFileOut <- argsMaybeOutFileBaseName . arguments <$> ask
  case mFileOut of
    Nothing -> logLBSQuiet $ L.intercalate (L.pack "\n") files
    Just fn -> do
      let nDigits    = ceiling $ logBase (10 :: Double) (fromIntegral m)
          digitStr i = L.unpack $ alignRightWith '0' nDigits (L.pack $ show i)
          fns = [ fn ++ digitStr i ++ ".fasta" | i <- [0 .. m-1] ]
      lift $ mapM_ (\i -> withFile (fns!!i) WriteMode (`L.hPutStr` (files!!i))) [0 .. m-1]
      logS $ "Results written to files with basename '" ++ fn ++ "'."

translateS :: Seq ()
translateS = do
  args <- arguments <$> ask
  let (Translate rf uc) = argsCommand args
  logS "Translate sequences to amino acids."
  logS $ "Universal code: " ++ show uc ++ "."
  logS ""
  sss <- readSeqss
  io $ L.intercalate (L.pack "\n") $
    map (sequencesToFasta . map (translateSeq uc rf)) sss

io :: L.ByteString -> Seq ()
io res = do
  mOutFileBaseName <- argsMaybeOutFileBaseName . arguments <$> ask
  cmd <- argsCommand . arguments <$> ask
  case mOutFileBaseName of
    Nothing -> logLBSQuiet res
    Just fn -> do
      let fn' = case cmd of
            Examine _ -> fn ++ ".out"
            _         -> fn ++ ".fasta"
      lift $ withFile fn' WriteMode (`L.hPutStr` res)
      logS $ "Results written to file '" ++ fn' ++ "'."

readSeqs :: Alphabet -> FilePath -> IO [Sequence]
readSeqs a = parseFileWith (fasta a)

readSeqss :: Seq [[Sequence]]
readSeqss = do
  fns <- argsFileNames . arguments <$> ask
  code <- argsCode . arguments <$> ask
  lift $ mapM (readSeqs code) fns

work :: Seq ()
work = do
  args <- arguments <$> ask
  lift programHeader >>= logS
  let c = argsCode args
  logS $ "Read fasta file(s); code " ++ show c ++ "."
  logS ""
  case argsCommand args of
    Examine{}       -> examineS
    Concatenate     -> concatenateS
    FilterRows{}    -> filterRowsS
    FilterColumns{} -> filterColumnsS
    SubSample{}     -> subSampleS
    Translate{}     -> translateS

main :: IO ()
main = do
  a <- parseArgs
  h <- setupLogger (argsVerbosity a) (argsMaybeOutFileBaseName a)
  runReaderT work (Params a h)
  closeLogger h
