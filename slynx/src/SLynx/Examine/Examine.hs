{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
--
-- Module      :  Analyze.Analyze
-- Description :  Parse sequence file formats and analyze them
-- Copyright   :  (c) Dominik Schrempf 2018
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 08:41:05 2018.
module SLynx.Examine.Examine
  ( examineCmd,
  )
where

import Control.Monad.Logger
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import qualified ELynx.Data.Alphabet.Alphabet as A
import qualified ELynx.Data.Alphabet.Character as C
import qualified ELynx.Data.Sequence.Alignment as M
import qualified ELynx.Data.Sequence.Sequence as Seq
import ELynx.Tools
import SLynx.Examine.Options
import SLynx.Tools
import Text.Printf

pRow :: String -> String -> BL.ByteString
pRow name val = alignLeft 50 n <> alignRight 10 v
  where
    n = BL.pack name
    v = BL.pack val

examineAlignment :: Bool -> M.Alignment -> BL.ByteString
examineAlignment perSiteFlag a =
  BL.unlines
    [ BL.pack
        "Sequences have equal length (multi sequence alignment, or single sequence).",
      pRow "Total number of columns in alignment:" $ show (M.length a),
      pRow "Number of columns without gaps:" $ show (M.length aNoGaps),
      pRow "Number of columns with standard characters only:" $
        show (M.length aOnlyStd),
      BL.empty,
      pRow "Total number of characters:" $ show nTot,
      pRow "Standard (i.e., not extended IUPAC) characters:" $
        show (nTot - nIUPAC - nGaps - nUnknowns),
      pRow "Extended IUPAC characters:" $ show nIUPAC,
      pRow "Gaps:" $ show nGaps,
      pRow "Unknowns:" $ show nUnknowns,
      BL.empty,
      pRow "Percentage of standard characters:" $
        printf "%2.2f" (100.0 - percentIUPAC - percentGaps - percentUnknowns),
      pRow "Percentage of extended IUPAC characters:" $
        printf "%2.2f" percentIUPAC,
      pRow "Percentage of gaps:" $ printf "%2.2f" percentGaps,
      pRow "Percentage of unknowns:" $ printf "%2.2f" percentUnknowns,
      BL.empty,
      BL.pack "Distribution of characters:",
      -- Holy crap.
      BL.pack $
        concatMap ((: "     ") . C.toChar) $
          S.toList $
            A.std $
              A.alphabetSpec $
                M.alphabet a,
      BL.pack $ unwords $ map (printf "%.3f") charFreqs,
      BL.empty,
      BL.pack "Mean effective number of states (measured using entropy):",
      pRow "Across whole alignment:" $ printf "%.3f" kEffMean,
      pRow "Across columns without gaps:" $ printf "%.3f" kEffMeanNoGaps,
      pRow "Across columns without extended IUPAC characters:" $
        printf "%.3f" kEffMeanOnlyStd,
      BL.empty,
      BL.pack "Mean effective number of states (measured using homoplasy):",
      pRow "Across whole alignment:" $ printf "%.3f" kEffMeanHomo,
      pRow "Across columns without gaps:" $ printf "%.3f" kEffMeanNoGapsHomo,
      pRow "Across columns without extended IUPAC characters:" $
        printf "%.3f" kEffMeanOnlyStdHomo
    ]
    <> perSiteBS
  where
    nTot = M.length a * M.nSequences a
    nIUPAC = M.countIUPACChars a
    nGaps = M.countGaps a
    nUnknowns = M.countUnknowns a
    percentIUPAC = 100 * fromIntegral nIUPAC / fromIntegral nTot :: Double
    percentGaps = 100 * fromIntegral nGaps / fromIntegral nTot :: Double
    percentUnknowns = 100 * fromIntegral nUnknowns / fromIntegral nTot :: Double
    aNoGaps = M.filterColsNoGaps a
    aOnlyStd = M.filterColsOnlyStd aNoGaps
    charFreqsPerSite = M.toFrequencyData a
    charFreqs = M.distribution charFreqsPerSite
    kEffs = M.kEffEntropy charFreqsPerSite
    kEffsNoGaps = M.kEffEntropy . M.toFrequencyData $ aNoGaps
    kEffsOnlyStd = M.kEffEntropy . M.toFrequencyData $ aOnlyStd
    kEffMean = sum kEffs / fromIntegral (length kEffs)
    kEffMeanNoGaps = sum kEffsNoGaps / fromIntegral (length kEffsNoGaps)
    kEffMeanOnlyStd = sum kEffsOnlyStd / fromIntegral (length kEffsOnlyStd)
    kEffsHomo = M.kEffHomoplasy charFreqsPerSite
    kEffsNoGapsHomo = M.kEffHomoplasy . M.toFrequencyData $ aNoGaps
    kEffsOnlyStdHomo = M.kEffHomoplasy . M.toFrequencyData $ aOnlyStd
    kEffMeanHomo = sum kEffsHomo / fromIntegral (length kEffsHomo)
    kEffMeanNoGapsHomo =
      sum kEffsNoGapsHomo / fromIntegral (length kEffsNoGapsHomo)
    kEffMeanOnlyStdHomo =
      sum kEffsOnlyStdHomo / fromIntegral (length kEffsOnlyStdHomo)
    perSiteBS =
      if perSiteFlag
        then
          BL.unlines
            [ BL.pack "Effective number of used states per site:",
              BL.pack . show $ kEffs
            ]
        else BL.empty

examine :: Bool -> [Seq.Sequence] -> BL.ByteString
examine perSiteFlag ss =
  Seq.summarizeSequences ss <> case M.fromSequences ss of
    Left _ -> BL.empty
    Right a -> BL.pack "\n" <> examineAlignment perSiteFlag a

-- | Examine sequences.
examineCmd :: ELynx ExamineArguments ()
examineCmd = do
  (ExamineArguments al inFile perSiteFlag) <- local <$> ask
  $(logInfo) "Command: Examine sequences."
  ss <- readSeqs al inFile
  let result = examine perSiteFlag ss
  out "result of examination" result ".out"
