{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  TLynx.Coalesce.Options
Description :  Argument parser for @slynx coalesce@
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May  3 11:51:07 2019.

-}

module TLynx.Coalesce.Options
  ( CoalesceArguments(..)
  , coalesceArguments
  , reportCoalesceArguments
  , coalesceFooter
  )
where

import           Data.List
import           Options.Applicative

import           ELynx.Tools

-- | Arguments need to simulate phylogenetic trees using the coalescent process.
data CoalesceArguments = CoalesceArguments
  { argsNTrees  :: Int            -- ^ Number of simulated trees.
  , argsNLeaves :: Int            -- ^ Number of leaves per tree.
  , argsRho     :: Maybe Double   -- ^ Perform random sub-sampling with given sampling rate.
  , argsSumStat :: Bool           -- ^ Only print summary statistics?
  , argsSeed    :: Seed           -- ^ Seed of NRG, random if 'Nothing'.
  }
  deriving (Eq, Show, Generic)

instance Reproducible CoalesceArguments where
  inFiles _ = []
  outSuffixes _ = [".tree"]
  getSeed = Just . argsSeed
  setSeed a s = a { argsSeed = Fixed s }
  parser  = coalesceArguments
  cmdName = "coalesce"
  cmdDsc =
    [ "Simulate phylogenetic trees using the coalescent processes (see also the 'simulate' command for simulations using the birth and death process)."
    ]
  cmdFtr = coalesceFooter

instance FromJSON CoalesceArguments

instance ToJSON CoalesceArguments

-- | Print useful information about the provided arguments.
reportCoalesceArguments :: CoalesceArguments -> String
reportCoalesceArguments a = intercalate
  "\n"
  [ "Number of simulated trees: " ++ show (argsNTrees a)
  , "Number of leaves per tree: " ++ show (argsNLeaves a)
  , "Sub-sampling: " ++ ssStr
  , "Summary statistics only: " ++ show (argsSumStat a)
  ]
 where
  ssStr = case argsRho a of
    Nothing -> "No"
    Just r  -> "Yes, with probability " <> show r

-- | Command line parser.
coalesceArguments :: Parser CoalesceArguments
coalesceArguments =
  CoalesceArguments
    <$> nTreeOpt
    <*> nLeavesOpt
    <*> rhoOpt
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

rhoOpt :: Parser (Maybe Double)
rhoOpt =
  optional
    $  option auto
    $  long "sub-sample"
    <> short 'r'
    <> metavar "DOUBLE"
    <> showDefault
    <> help
         "Perform random sub-sampling with sampling probability rho (see below)"

sumStatOpt :: Parser Bool
sumStatOpt =
  switch $ long "summary-statistics" <> short 's' <> showDefault <> help
    "Only output number of children for each branch"

-- | And a footer.
coalesceFooter :: [String]
coalesceFooter =
  [ "Sub-sampling: simulate one big tree with n'=round(n/rho), n'>=n, leaves, and randomly sample sub-trees with n leaves. Hence, with rho=1.0, the same tree is reported over and over again."
  , "Summary statistics: only print (NumberOfExtantChildren BranchLength) pairs for each branch of each tree. The trees are separated by a newline character."
  ]
