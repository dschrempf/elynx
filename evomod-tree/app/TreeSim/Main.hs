{- |
   Description :  Simulate reconstructed trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Tue Feb 27 17:27:16 2018.

See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.

TODO: lambda ~ mu.

Mon Feb 4 14:26:11 CET 2019: Adding sampling probability rho. See Article
(Stadler2009) Stadler, T. On incomplete sampling under birth–death models and
connections to the sampling-based coalescent Journal of Theoretical Biology,
Elsevier BV, 2009, 261, 58-66

-}

module Main where

import           Control.Concurrent              (getNumCapabilities,
                                                  myThreadId, threadCapability)
import           Control.Concurrent.Async        (replicateConcurrently)
import           Control.Monad                   (replicateM, unless, when)
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8      as L
import           Data.Semigroup                  ((<>))
import           Data.Tree
import           Data.Vector                     (singleton)
import           Data.Version                    (showVersion)
import           Options.Applicative
import           Options.Applicative.Help.Pretty
import           Paths_evomod_tree               (version)
import qualified System.Environment              as Sys
import           System.Random.MWC

import           EvoMod.Data.Tree.PhyloTree      (PhyloIntLabel)
import           EvoMod.Data.Tree.SumStat        (formatNChildSumStat,
                                                  toNChildSumStat)
import           EvoMod.Export.Tree.Newick       (toNewickPhyloIntTree)
import           EvoMod.Simulate.PointProcess    (simulateReconstructedTree)

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
  , seed      :: Maybe Int -- ^ Seed of NRG, random if 'Nothing'.
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

-- | The impure IO action that reads the arguments and prints out help if
  -- needed.
parseArgs :: IO Args
parseArgs = do
  a <- execParser $
    info (helper <*> argsParser)
    (fullDesc
     <> header "Simulate reconstructed trees"
     <> progDesc desc
     <> footerDoc remarks )
  if verbosity a && quiet a
    then error "Cannot be verbose and quiet at the same time."
    else return a
  where
    desc = "Simulate reconstructed trees using the point process. See Gernhard, T. (2008). The conditioned reconstructed process. Journal of Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005"
    remarks = Just $ foldl1 (.$.) (map text strs)
    strs    = [ "Height of Trees: If no tree height is given, the heights will be randomly drawn from the expected distribution given the number of leaves, the birth and the death rate."
              , "Summary statistics only: Only print (NumberOfExtantChildren BranchLength) pairs for each branch of each tree. The trees are separated by a newline character."]

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

seedOpt :: Parser (Maybe Int)
seedOpt = optional $ option auto
  ( long "seed"
    <> short 'S'
    <> metavar "INT"
    <> help "Seed for random number generator (default: random)" )

getCommandLineStr :: String -> [String] -> String
getCommandLineStr n as = unlines
  [ "Reconstructed trees simulator version " ++ showVersion version ++ "."
  , "Command line: " ++ n ++ " " ++ unwords as ]

newSection :: String -> String
newSection h = unlines
  [ ""
  , "-- " ++ h ]

main :: IO ()
main = do
  args <- parseArgs
  let v = verbosity args
      q = quiet args
      s = sumStat args
  c <- getNumCapabilities
  unless q $ do
    p <- Sys.getProgName
    a <- Sys.getArgs
    putStr $ getCommandLineStr p a
    putStr $ newSection "Arguments"
    putStr $ reportArgs args
    putStr $ newSection "Simulation"
  when v $ putStrLn $ "Number of used cores: " ++ show c
  trs <- simulateNTreesConcurrently c args
  let ls = if s
           then parMap rpar (formatNChildSumStat . toNChildSumStat) trs
           else parMap rpar toNewickPhyloIntTree trs
  L.putStr $ L.unlines ls

simulateNTreesConcurrently :: Int -> Args -> IO [Tree PhyloIntLabel]
simulateNTreesConcurrently c (Args t n h l m r _ v _ s) = do
  -- when (l <= 0) (error "Speciation rate has to be larger than zero.")
  -- when (m <= 0) (error "Extinction rate has to be larger than zero.")
  -- when ((r <= 0) || (r > 1)) (error "Sampling probability has to in (0,1].")
  let l' = l * r
      m' = m - l * (1.0 - r)
  trsCon <- replicateConcurrently c (simulateNTrees (t `div` c) n h l' m' v s)
  trsRem <- simulateNTrees (t `mod` c) n h l' m' v s
  return $ concat trsCon ++ trsRem

simulateNTrees :: Int -> Int -> Maybe Double -> Double -> Double -> Bool
               -> Maybe Int
               -> IO [Tree PhyloIntLabel]
simulateNTrees t n mH l m v s
  | t <= 0 = return []
  | otherwise = do
      when v reportCapability
      g <- maybe createSystemRandom (initialize . singleton . fromIntegral) s
      let f = simulateReconstructedTree n mH l m g
      replicateM t f

-- simulateNBranchLengthNChildrenConcurrently :: Int -> Args -> IO T.Text
-- simulateNBranchLengthNChildrenConcurrently c (Args t n h l m _ v _ s) = do
--   trsCon <- replicateConcurrently c (simulateNBranchLengthNChildren (t `div` c) n h l m v s)
--   trsRem <- simulateNBranchLengthNChildren (t `mod` c) n h l m v s
--   let trs = concat trsCon ++ trsRem
--       ls  = parMap rpar formatNChildSumStat trs
--   return $ T.unlines ls


-- simulateNBranchLengthNChildren :: Int -> Int -> Maybe Double -> Double -> Double -> Bool
--                                -> Maybe Int
--                                -> IO [[(Double, Int)]]
-- simulateNBranchLengthNChildren t n mH l m v s = do
--   when v reportCapability
--   g <- maybe createSystemRandom (initialize . singleton . fromIntegral) s
--   let f = case mH of
--             Nothing -> simulateBranchLengthNChildrenRandomHeight n l m g
--             Just h  -> simulateBranchLengthNChildren n h l m g
--   replicateM t f

reportCapability :: IO ()
reportCapability = do
  i <- myThreadId
  (c, _) <- threadCapability i
  putStrLn $ "Running on core: " ++ show c
