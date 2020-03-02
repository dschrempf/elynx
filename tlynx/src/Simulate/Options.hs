{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  Simulate.Options
Description :  Argument parser for seq-ana
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May  3 11:51:07 2019.

-}

module Simulate.Options
  ( SimulateArguments(..)
  , simulateArguments
  , reportSimulateArguments
  , simulateFooter
  )
where

import           Data.List
import           Options.Applicative

import           ELynx.Tools.Reproduction

-- | Arguments need to simulate phylogenetic trees using birth and death processes.
data SimulateArguments = SimulateArguments
  { argsNTrees        :: Int            -- ^ Simulated trees.
  , argsNLeaves       :: Int            -- ^ Number of leaves.
  , argsHeight        :: Maybe Double   -- ^ Tree height (time to origin or MRCA).
  , argsConditionMRCA :: Bool           -- ^ False: condition on origin; True:
                                        -- condition on MRCA.
  , argsLambda        :: Double         -- ^ Birth rate.
  , argsMu            :: Double         -- ^ Death rate.
  , argsRho           :: Double         -- ^ Smapling rate.
  , argsSubSample     :: Bool           -- ^ Perform actual sub-sampling.
  , argsSumStat       :: Bool           -- ^ Only print summary statistics?
  , argsSeed          :: Seed           -- ^ Seed of NRG, random if 'Nothing'.
  }
  deriving (Eq, Show, Generic)

instance Reproducible SimulateArguments where
  inFiles _ = []
  getSeed = Just . argsSeed
  setSeed a s = a { argsSeed = Fixed s }
  parser _ = simulateArguments

instance ToJSON SimulateArguments

-- | Print useful information about the provided arguments.
reportSimulateArguments :: SimulateArguments -> String
reportSimulateArguments a = intercalate
  "\n"
  [ "Number of simulated trees: " ++ show (argsNTrees a)
  , "Number of leaves per tree: " ++ show (argsNLeaves a)
  , "Height of trees: " ++ hStr
  , "Birth rate: " ++ show (argsLambda a)
  , "Death rate: " ++ show (argsMu a)
  , "Sampling probability: " ++ show (argsRho a)
  , "Perform sub-sampling: " ++ show (argsSubSample a)
  , "Summary statistics only: " ++ show (argsSumStat a)
  , "Seed: " ++ show (argsSeed a)
  ]
 where
  hStr = case argsHeight a of
    Nothing -> "Random height of origin"
    Just h  -> show h ++ ", conditioned on " ++ if argsConditionMRCA a
      then "MRCA"
      else "origin"

-- | Command line parser.
simulateArguments :: Parser SimulateArguments
simulateArguments =
  SimulateArguments
    <$> nTreeOpt
    <*> nLeavesOpt
    <*> treeHeightOpt
    <*> conditionMRCAOpt
    <*> lambdaOpt
    <*> muOpt
    <*> rhoOpt
    <*> subSampleOpt
    <*> sumStatOpt
    <*> seedOpt

nTreeOpt :: Parser Int
nTreeOpt =
  option auto
    $  long "nTrees"
    <> short 't'
    <> metavar "INT"
    <> value 10
    <> showDefault
    <> help "Number of trees"

nLeavesOpt :: Parser Int
nLeavesOpt =
  option auto
    $  long "nLeaves"
    <> short 'n'
    <> metavar "INT"
    <> value 5
    <> showDefault
    <> help "Number of leaves per tree"

treeHeightOpt :: Parser (Maybe Double)
treeHeightOpt =
  optional
    $  option auto
    $  long "height"
    <> short 'H'
    <> metavar "DOUBLE"
    <> help "Fix tree height (no default)"

conditionMRCAOpt :: Parser Bool
conditionMRCAOpt =
  switch $ long "condition-on-mrca" <> short 'M' <> showDefault <> help
    "Do not condition on height of origin but on height of MRCA"

lambdaOpt :: Parser Double
lambdaOpt =
  option auto
    $  long "lambda"
    <> short 'l'
    <> metavar "DOUBLE"
    <> value 1.0
    <> showDefault
    <> help "Birth rate lambda"

muOpt :: Parser Double
muOpt =
  option auto
    $  long "mu"
    <> short 'm'
    <> metavar "DOUBLE"
    <> value 0.9
    <> showDefault
    <> help "Death rate mu"

rhoOpt :: Parser Double
rhoOpt =
  option auto $ long "rho" <> short 'r' <> metavar "DOUBLE" <> value 1.0 <> help
    "Sampling probability rho (default: 1.0)"

subSampleOpt :: Parser Bool
subSampleOpt = switch $ long "sub-sample" <> short 'u' <> showDefault <> help
  "Perform sub-sampling; see below."

sumStatOpt :: Parser Bool
sumStatOpt =
  switch $ long "summary-statistics" <> short 's' <> showDefault <> help
    "Only output number of children for each branch"

citation :: String
citation =
  "Gernhard, T. (2008). The conditioned reconstructed process. Journal of Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005"

-- | And a footer.
simulateFooter :: String
simulateFooter = intercalate
  "\n"
  [ "Height of Trees: if no tree height is given, the heights will be randomly drawn from the expected distribution given the number of leaves, the birth and the death rate."
  , "Summary statistics only: only print (NumberOfExtantChildren BranchLength) pairs for each branch of each tree. The trees are separated by a newline character."
  , "Sub-sampling: simulate one big tree with n'=round(n/rho), n'>=n, leaves, and randomly sample sub-trees with n leaves. Hence, with rho=1.0, the same tree is reported over and over again."
  , citation
  ]
