{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |

Module      :  Analyze.Analyze
Description :  Parse sequence file formats and analyze them
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 08:41:05 2018.

-}

module Examine.Examine
  ( examineCmd
  )
where

import           Control.Monad.Logger
import           Control.Monad.Trans.Reader     ( ask )
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.Set                      as S
import           Text.Printf

import           Examine.Options
import           Tools

import qualified ELynx.Data.Alphabet.Alphabet  as A
import qualified ELynx.Data.Alphabet.Character as C
import qualified ELynx.Data.Sequence.Alignment as M
import qualified ELynx.Data.Sequence.Sequence  as Seq
import           ELynx.Tools

examineAlignment :: Bool -> M.Alignment -> L.ByteString
examineAlignment perSiteFlag a =
  L.unlines
      [ L.pack
        "Sequences have equal length (multi sequence alignment, or single sequence)."
      , L.pack $ "Total number of columns in alignment: " ++ show (M.length a)
      , L.pack $ "Number of columns without gaps: " ++ show (M.length aNoGaps)
      , L.pack $ "Number of columns with standard characters only: " ++ show
        (M.length aOnlyStd)
      , L.empty
      , L.pack $ "Total number of characters: " ++ show nTot
      , L.pack $ "Standard (i.e., not extended IUPAC) characters: " ++ show
        (nTot - nIUPAC - nGaps - nUnknowns)
      , L.pack $ "Extended IUPAC characters: " ++ show nIUPAC
      , L.pack $ "Gaps: " ++ show nGaps
      , L.pack $ "Unknowns: " ++ show nUnknowns
      , L.pack $ "Percentage of standard characters: " ++ printf
        "%.3f"
        (1.0 - percentageIUPAC - percentageGaps - percentageUnknowns)
      , L.pack
      $  "Percentage of extended IUPAC characters: "
      ++ printf "%.3f" percentageIUPAC
      , L.pack $ "Percentage of gaps: " ++ printf "%.3f" percentageGaps
      , L.pack $ "Percentage of unknowns: " ++ printf "%.3f" percentageUnknowns
      , L.empty
      , L.pack "Distribution of characters:"
              -- Holy crap.
      , L.pack
      $ concatMap ((: "     ") . C.toChar)
      $ S.toList
      $ A.std
      $ A.alphabetSpec
      $ M.alphabet a
      , L.pack $ unwords $ map (printf "%.3f") charFreqs
      , L.empty
      , L.pack "Mean effective number of states (measured using entropy):"
      , L.pack "Across whole alignment: " <> L.pack (printf "%.3f" kEffMean)
      , L.pack "Across columns without gaps: "
        <> L.pack (printf "%.3f" kEffMeanNoGaps)
      , L.pack "Across columns without extended IUPAC characters: "
        <> L.pack (printf "%.3f" kEffMeanOnlyStd)
      , L.pack "Mean effective number of states (measured using homoplasy):"
      , L.pack "Across whole alignment: " <> L.pack (printf "%.3f" kEffMeanHomo)
      , L.pack "Across columns without gaps: "
        <> L.pack (printf "%.3f" kEffMeanNoGapsHomo)
      , L.pack "Across columns without extended IUPAC characters: "
        <> L.pack (printf "%.3f" kEffMeanOnlyStdHomo)
      ]
    <> perSiteBS
 where
  nTot                = M.length a * M.nSequences a
  nIUPAC              = M.countIUPACChars a
  nGaps               = M.countGaps a
  nUnknowns           = M.countUnknowns a
  percentageIUPAC     = fromIntegral nIUPAC / fromIntegral nTot :: Double
  percentageGaps      = fromIntegral nGaps / fromIntegral nTot :: Double
  percentageUnknowns  = fromIntegral nUnknowns / fromIntegral nTot :: Double
  aNoGaps             = M.filterColsNoGaps a
  aOnlyStd            = M.filterColsOnlyStd aNoGaps
  charFreqsPerSite    = M.toFrequencyData a
  charFreqs           = M.distribution charFreqsPerSite
  kEffs               = M.kEffEntropy charFreqsPerSite
  kEffsNoGaps         = M.kEffEntropy . M.toFrequencyData $ aNoGaps
  kEffsOnlyStd        = M.kEffEntropy . M.toFrequencyData $ aOnlyStd
  kEffMean            = sum kEffs / fromIntegral (length kEffs)
  kEffMeanNoGaps      = sum kEffsNoGaps / fromIntegral (length kEffsNoGaps)
  kEffMeanOnlyStd     = sum kEffsOnlyStd / fromIntegral (length kEffsOnlyStd)
  kEffsHomo           = M.kEffHomoplasy charFreqsPerSite
  kEffsNoGapsHomo     = M.kEffHomoplasy . M.toFrequencyData $ aNoGaps
  kEffsOnlyStdHomo    = M.kEffHomoplasy . M.toFrequencyData $ aOnlyStd
  kEffMeanHomo        = sum kEffsHomo / fromIntegral (length kEffsHomo)
  kEffMeanNoGapsHomo  = sum kEffsNoGapsHomo / fromIntegral (length kEffsNoGapsHomo)
  kEffMeanOnlyStdHomo = sum kEffsOnlyStdHomo / fromIntegral (length kEffsOnlyStdHomo)
  perSiteBS           = if perSiteFlag
    then L.unlines
      [ L.pack "Effective number of used states per site:"
      , L.pack . show $ kEffs
      ]
    else L.empty

examine :: Bool -> [Seq.Sequence] -> L.ByteString
examine perSiteFlag ss =
  Seq.summarizeSequences ss <> case M.fromSequences ss of
    Left  _ -> L.empty
    Right a -> L.pack "\n" <> examineAlignment perSiteFlag a

-- | Examine sequences.
examineCmd :: ELynx ExamineArguments ()
examineCmd = do
  (ExamineArguments al inFile perSiteFlag) <- local <$> ask
  $(logInfo) "Command: Examine sequences."
  ss <- readSeqs al inFile
  let result = examine perSiteFlag ss
  out "result of examination" result ".out"
