{- |
Module      :  Options
Description :  SLynx general options
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sat Sep  7 18:55:03 2019.

-}

module Options
  (
  ) where



-- data CommandArguments =
--   Examine
--     { exPerSite :: Bool
--     , exInFile    :: Maybe FilePath }
--   | Concatenate
--     { ccInFiles         :: [FilePath] }
--   | FilterRows
--     { frLonger  :: Maybe Int
--     , frShorter :: Maybe Int
--     , frInFile    :: Maybe FilePath }
--   | FilterColumns
--     { fcStandard :: Maybe Double
--     , fcInFile     :: Maybe FilePath }
--   | SubSample
--     { ssNSites      :: Int
--     , ssNAlignments :: Int
--     , ssMbSeed      :: Maybe [Word32]
--     , ssInFile        :: Maybe FilePath }
--   | Translate
--     { trReadingFrame  :: Int
--     , trUniversalCode :: UniversalCode
--     , trInFile          :: Maybe FilePath }

-- data Arguments = Arguments { globalArgs  :: GlobalArguments
--                            , alphabetArg :: Alphabet
--                            , commandArgs :: CommandArguments }

-- arguments :: Parser Arguments
-- arguments = Arguments
--   <$> globalArguments
--   <*> alphabetOpt
--   <*> commandArguments

-- commandArguments :: Parser CommandArguments
-- commandArguments = hsubparser $
--   examineCommand <>
--   concatenateCommand <>
--   filterRowsCommand <>
--   filterColumnsCommand <>
--   subSampleCommand <>
--   translateCommand

-- parseArguments :: IO Arguments
-- parseArguments = parseArgumentsWith desc ftr arguments

-- desc :: [String]
-- desc = [ "Analyze multi sequence alignments." ]

-- ftr :: [String]
-- ftr = [ "File formats:" ] ++ fs ++
--       [ "", "Alphabet types:" ] ++ as
--   where
--     toListItem = ("  - " ++)
--     fs = map toListItem ["FASTA"]
--     as = map (toListItem . alphabetNameVerbose) [(minBound :: Alphabet) ..]
