{- |
Module      :  ArgParseTreeSim
Description :  Argument parser for seq-ana.
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May  3 11:51:07 2019.

-}

module ArgParseTreeSim
  ( Args (..)
  , reportArgs
  , parseArgs
  ) where

import           Data.Word
import           Options.Applicative

import           EvoMod.Definitions

data Args = Args
  { nTrees    :: Int    -- ^ Simulated trees.
  , nLeaves   :: Int    -- ^ Number of leaves.
  , height    :: Maybe Double -- ^ Tree height (time to origin).
  , lambda    :: Double -- ^ Birth rate.
  , mu        :: Double -- ^ Death rate.
  , rho       :: Double -- ^ Smapling rate.
  , sumStat   :: Bool   -- ^ Only print summary statistics?
  , verbosity :: Bool   -- ^ Verbosity.
  , quiet     :: Bool   -- ^ Be quiet?
  , seed      :: Maybe [Word32] -- ^ Seed of NRG, random if 'Nothing'.
  }

reportArgs :: Args -> String
reportArgs a =
  unlines [ "Number of simulated trees: " ++ show (nTrees a)
          , "Number of leaves per tree: " ++ show (nLeaves a)
          , "Height of trees: " ++ hStr
          , "Birth rate: " ++ show (lambda a)
          , "Death rate: " ++ show (mu a)
          , "Sampling probability: " ++ show (rho a)
          , "Summary statistics only: " ++ show (sumStat a)
          , "Verbosity: " ++ show (verbosity a)
          , "Quiet: " ++ show (quiet a)
          , "Seed: " ++ sStr ]
  where hStr = case height a of Nothing -> "Random"
                                Just h  -> show h
        sStr = case seed a of Nothing -> "Random"
                              Just i  -> show i

argsParser :: Parser Args
argsParser = Args
  <$> nTreeOpt
  <*> nLeavesOpt
  <*> treeHeightOpt
  <*> lambdaOpt
  <*> muOpt
  <*> rhoOpt
  <*> sumStatOpt
  <*> verbosityOpt
  <*> quietOpt
  <*> seedOpt

nTreeOpt :: Parser Int
nTreeOpt = option auto
  ( long "nTrees"
    <> short 't'
    <> metavar "INT"
    <> value 10
    <> showDefault
    <> help "Number of trees" )

nLeavesOpt :: Parser Int
nLeavesOpt = option auto
  ( long "nLeaves"
    <> short 'n'
    <> metavar "INT"
    <> value 5
    <> showDefault
    <> help "Number of leaves per tree" )

treeHeightOpt :: Parser (Maybe Double)
treeHeightOpt = optional $ option auto
  ( long "height"
    <> short 'H'
    <> metavar "DOUBLE"
    <> help "Fix tree height (no default)" )


lambdaOpt :: Parser Double
lambdaOpt = option auto
  ( long "lambda"
    <> short 'l'
    <> metavar "DOUBLE"
    <> value 1.0
    <> showDefault
    <> help "Birth rate lambda" )

muOpt :: Parser Double
muOpt = option auto
  ( long "mu"
    <> short 'm'
    <> metavar "DOUBLE"
    <> value 0.9
    <> showDefault
    <> help "Death rate mu" )

rhoOpt :: Parser Double
rhoOpt = option auto
  ( long "rho"
    <> short 'r'
    <> metavar "DOUBLE"
    <> value 1.0
    <> help "Sampling probability rho (default: 1.0)" )

sumStatOpt :: Parser Bool
sumStatOpt = switch
  ( long "summary-statistics"
    <> short 's'
    <> showDefault
    <> help "Only output number of children for each branch" )

verbosityOpt :: Parser Bool
verbosityOpt = switch
  ( long "verbosity"
    <> short 'v'
    <> showDefault
    <> help "Be verbose; incompatible with -q" )

quietOpt :: Parser Bool
quietOpt = switch
  ( long "quiet"
    <> short 'q'
    <> showDefault
    <> help "Be quiet; incompatible with -v" )

seedOpt :: Parser (Maybe [Word32])
seedOpt = optional $ option auto
  ( long "seed"
    <> short 'S'
    <> metavar "[INT]"
    <> help ("Seed for random number generator; "
             ++ "list of 32 bit integers with up to 256 elements (default: random)" ) )

-- getCommandLineStr :: String -> [String] -> String
-- getCommandLineStr n as = unlines
--   [ "Reconstructed trees simulator version " ++ showVersion version ++ "."
--   , "Command line: " ++ n ++ " " ++ unwords as ]

hdr :: [String]
hdr = ["Simulate reconstructed trees using the point process. See Gernhard, T. (2008). The conditioned reconstructed process. Journal of Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005"]

ftr :: [String]
ftr = [ "Height of Trees: If no tree height is given, the heights will be randomly drawn from the expected distribution given the number of leaves, the birth and the death rate."
              , "Summary statistics only: Only print (NumberOfExtantChildren BranchLength) pairs for each branch of each tree. The trees are separated by a newline character."]

-- | The impure IO action that reads the arguments and prints out help if
-- needed.
parseArgs :: IO Args
parseArgs = do
  a <- parseArgsWith (Just hdr) (Just ftr) argsParser
  if verbosity a && quiet a
    then error "Cannot be verbose and quiet at the same time."
    else return a
