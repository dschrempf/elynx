{- |
Module      :  Main
Description :  Parse sequence file formats and analyze them
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 08:41:05 2018.

XXX: Somehow this implementation still uses 2.5 times the memory that it
actually needs to use. I think that when parsing the sequences, the lines are
copied into the complete sequence (see the function 'fastaSequence').

XXX: Provide possibility to parse and handle sequences with different codes.

TODO: Use Quiet, Info, Debug.

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

import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence
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

examineSequences :: Bool -> [Sequence] -> L.ByteString
examineSequences perSiteFlag ss
  | equalLength ss = summarizeSequenceList ss
                     <> L.pack "\n"
                     <> examineMSA perSiteFlag (fromSequenceList ss)
  | otherwise      = summarizeSequenceList ss

examineMSA :: Bool -> MultiSequenceAlignment -> L.ByteString
examineMSA perSiteFlag msa =
  L.unlines [ L.pack $ "Total number of columns in alignment: "
              ++ show (msaLength msa)
            , L.pack $ "Number of columns without gaps or unknown characters: "
              ++ show (msaLength msaFltGaps)
            , L.pack $ "Number of columns without extended IUPAC characters: "
              ++ show (msaLength msaFltIUPAC)
            , L.empty
            , L.pack $ "Total chars: " ++ show nTot
            , L.pack $ "Standard (i.e., not extended IUPAC) characters: " ++ show nNonStd
            , L.pack $ "Non-standard (i.e., extended IUPAC) characters: " ++ show (nTot - nNonStd)
            , L.pack $ "Gaps or unknown characters: " ++ show nGaps
            , L.pack $ "Percentage of standard characters: "
              ++ printf "%.3f" percentageNonStd
            , L.pack $ "Percentage of non-standard characters: "
              ++ printf "%.3f" (1.0 - percentageNonStd)
            , L.pack $ "Percentage of gaps or unknown characters: "
              ++ printf "%.3f" percentageGaps
            , L.empty
            , L.pack "Mean effective number of used states:"
            , L.pack "Across whole alignment: "
              <> L.pack (show kEffMean)
            , L.pack "Across columns without extended IUPAC characters: "
              <> L.pack (show kEffMeanFltIUPAC)
            , L.pack "Across columns without gaps or unknown characters: "
              <> L.pack (show kEffMeanFltGaps)
            ]
  <> perSiteBS
  where
    nTot                = msaLength msa * msaNSequences msa
    nNonStd             = countStandardChars msa
    nGaps               = countGapOrUnknownChars msa
    percentageNonStd    = fromIntegral nNonStd / fromIntegral nTot :: Double
    percentageGaps      = fromIntegral nGaps   / fromIntegral nTot :: Double
    msaFltGaps          = filterColumnsGapsUnknowns msa
    msaFltIUPAC         = filterColumnsIUPAC msaFltGaps
    kEffs               = kEff . toFrequencyData $ msa
    kEffsFltGaps        = kEff . toFrequencyData $ msaFltGaps
    kEffsFltIUPAC       = kEff . toFrequencyData $ msaFltIUPAC
    kEffMean            = sum kEffs / fromIntegral (length kEffs)
    kEffMeanFltGaps     = sum kEffsFltGaps  / fromIntegral (length kEffsFltGaps)
    kEffMeanFltIUPAC    = sum kEffsFltIUPAC / fromIntegral (length kEffsFltIUPAC)
    perSiteBS           = if perSiteFlag
                          then L.unlines [ L.pack "Effective number of used states per site:"
                                         , L.pack . show $ kEffs
                                         ]
                          else L.empty

-- subsample nSites nSamples msa gen
subsample :: (PrimMonad m)
          => Int -> Int -> MultiSequenceAlignment -> Gen (PrimState m) -> m [MultiSequenceAlignment]
subsample n m msa g = replicateM m $ randomSubSample n msa g

act :: Command -> [[Sequence]] -> Seq ()
-- act Summarize sss      = io $ L.intercalate (L.pack "\n") $ map summarizeSequenceList sss
act (Examine perSiteFlag) sss = io $ L.intercalate (L.pack "\n") $
  map (examineSequences perSiteFlag) sss
act Concatenate sss    = io $ sequencesToFasta $ concatenateSeqs sss
act (Filter ml ms) sss = io $ sequencesToFasta $ compose filters $ concat sss
  where filters        = map (fromMaybe id) [ filterLongerThan <$> ml
                                    , filterShorterThan <$> ms ]
act (SubSample n m ms) sss = do
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
act (Translate rf uc) sss = do
  logS "Translate sequences to amino acids."
  logS $ "Universal code: " ++ show uc ++ "."
  io $ L.intercalate (L.pack "\n") $
    map (sequencesToFasta . map (translate rf uc)) sss

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

work :: Seq ()
work = do
  args <- arguments <$> ask
  lift programHeader >>= logS
  let c = argsCode args
  logS $ "Read fasta file(s); code " ++ show c ++ "."
  logS ""
  let fns = argsFileNames args
  -- 'sss' is a little weird, but it is a list of a list of sequences.
  sss <- lift $ sequence $ parseFileWith (fasta c) <$> fns
  act (argsCommand args) sss

main :: IO ()
main = do
  a <- parseArgs
  h <- setupLogger (argsVerbosity a) (argsMaybeOutFileBaseName a)
  runReaderT work (Params a h)
  closeLogger h
