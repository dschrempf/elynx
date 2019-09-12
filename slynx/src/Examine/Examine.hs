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
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8                 as L
import           Text.Printf

import           Examine.Options
import           Tools

import           ELynx.Data.Sequence.MultiSequenceAlignment
import           ELynx.Data.Sequence.Sequence
import           ELynx.Tools.InputOutput

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
            , L.pack "Mean effective number of states (measured using entropy):"
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
    kEffs               = kEffEntropy . toFrequencyData $ msa
    kEffsNoGaps         = kEffEntropy . toFrequencyData $ msaNoGaps
    kEffsOnlyStd        = kEffEntropy . toFrequencyData $ msaOnlyStd
    kEffMean            = sum kEffs / fromIntegral (length kEffs)
    kEffMeanNoGaps      = sum kEffsNoGaps  / fromIntegral (length kEffsNoGaps)
    kEffMeanOnlyStd     = sum kEffsOnlyStd / fromIntegral (length kEffsOnlyStd)
    perSiteBS           = if perSiteFlag
                          then L.unlines [ L.pack "Effective number of used states per site:"
                                         , L.pack . show $ kEffs
                                         ]
                          else L.empty

examine :: Bool -> [Sequence] -> L.ByteString
examine perSiteFlag ss = summarizeSequenceList ss <>
  case fromSequenceList ss of
    Left _    -> L.empty
    Right msa -> L.pack "\n" <> examineMSA perSiteFlag msa

-- | Examine sequences.
examineCmd :: Maybe FilePath -> Examine ()
examineCmd outFileBaseName = do
  $(logInfo) "Command: Examine sequences."
  ExamineArguments al inFile perSiteFlag <- lift ask
  ss <- readSeqs al inFile
  let result = examine perSiteFlag ss
  let outFilePath = (++ ".out") <$> outFileBaseName
  io "result of examination" result outFilePath
