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

module SubSample.SubSample
  ( subSampleCmd
  )
where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader     ( ask )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Builder        as LT
import qualified Data.Text.Lazy.Builder.Int    as LT
import           System.Random.MWC

import           SubSample.Options
import           Tools

import qualified ELynx.Data.Sequence.Alignment as M
import           ELynx.Export.Sequence.Fasta
import           ELynx.Tools.InputOutput
import           ELynx.Tools.Reproduction       ( ELynx
                                                , Arguments(..)
                                                , GlobalArguments(..)
                                                , Seed(..)
                                                )

-- | Get a given number of output file names with provided suffix.
--
-- > getOutFilePaths "BasePath" 11 "fasta"
--
-- Will result in @BasePath.00.fasta@ up to @BasePath.10.fasta@.
getOutFilePaths :: String -> Int -> String -> [FilePath]
getOutFilePaths file n suffix =
  [ file ++ "." ++ digitStr i ++ "." ++ suffix | i <- [0 .. n - 1] ]
 where
  nDigits = ceiling $ logBase (10 :: Double) (fromIntegral n)
  digitStr i = T.unpack
    $ T.justifyRight nDigits '0' (LT.toStrict $ LT.toLazyText $ LT.decimal i)

-- | Sub sample sequences.
subSampleCmd :: ELynx SubSampleArguments ()
subSampleCmd = do
  (SubSampleArguments al inFile nSites nAlignments (Fixed s)) <- local <$> ask
  $(logInfo) "Command: Sub sample from a multi sequence alignment."
  $(logInfo) $ T.pack $ "  Sample " <> show nSites <> " sites."
  $(logInfo)
    $  T.pack
    $  "  Sample "
    <> show nAlignments
    <> " multi sequence alignments."
  ss  <- readSeqs al inFile
  gen <- liftIO $ initialize s
  let a = either error id (M.fromSequences ss)
  samples <- liftIO $ replicateM nAlignments $ M.randomSubSample nSites a gen
  let results = map (sequencesToFasta . M.toSequences) samples
  bn           <- outFileBaseName . global <$> ask
  outFilePaths <- case bn of
    Nothing -> return $ repeat Nothing
    Just fn -> return $ Just <$> getOutFilePaths fn nAlignments "fasta"
  zipWithM_ (out "sub sampled multi sequence alignments") results outFilePaths
