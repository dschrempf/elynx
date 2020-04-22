{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  SLynx.Simulate.Options
Description :  ELynxSim argument parsing
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Oct  7 17:29:45 2018.

Available options:
  -h,--help                Show this help text
  -v,--version             Show version
  -t,--tree-file NAME      Specify tree file NAME
  -s,--substitution-model MODEL
                           Set the phylogenetic substitution model; available
                           models are shown below
  -m,--mixture-model MODEL Set the phylogenetic mixture model; available models
                           are shown below
  -l,--length NUMBER       Set alignment length to NUMBER
  -e,--edm-file NAME       empirical distribution model file NAME in Phylobayes
                           format
  -w,--mixture-model-weights [DOUBLE,DOUBLE,...]
                           weights of mixture model components
  -g,--gamma-rate-heterogeneity (NCAT, SHAPE)
                           number of gamma rate categories and shape parameter
  -e,--seed [INT]            Set seed for the random number generator; list of 32
                           bit integers with up to 256 elements (default: [0])
  -q,--quiet               Be quiet
  -o,--output-file NAME    Specify output file NAME


-}


module SLynx.Simulate.Options
  ( GammaRateHeterogeneityParams
  , SimulateArguments(..)
  , simulateArguments
  , simulateFooter
  )
where

import           Data.Maybe                     ( maybeToList
                                                , fromMaybe
                                                )
import           Data.List
import           Options.Applicative

import           ELynx.Tools

-- | Number of gamma rate categories and alpha parameter.
type GammaRateHeterogeneityParams = (Int, Double)

-- | Arguments needed to simulate sequences.
data SimulateArguments = SimulateArguments
  { argsTreeFile                :: FilePath
  , argsSubstitutionModelString :: Maybe String
  , argsMixtureModelString      :: Maybe String
  , argsEDMFile                 :: Maybe FilePath
  , argsSiteprofilesFiles       :: Maybe [FilePath]
  , argsMixtureWeights          :: Maybe [Double]
  , argsGammaParams             :: Maybe GammaRateHeterogeneityParams
  , argsLength                  :: Int
  , argsSeed                    :: Seed
  }
  deriving (Eq, Show, Generic)

instance Reproducible SimulateArguments where
  inFiles a =
    argsTreeFile a
      : (maybeToList (argsEDMFile a) ++ fromMaybe [] (argsSiteprofilesFiles a))
  outSuffixes _ = [".model.gz", ".fasta"]
  getSeed = Just . argsSeed
  setSeed a s = a { argsSeed = Fixed s }
  parser  = simulateArguments
  cmdName = "simulate"
  cmdDesc = "Simulate multi sequence alignments."
  cmdFtr  = Just simulateFooter

instance ToJSON SimulateArguments

-- | Sub command parser.
simulateArguments :: Parser SimulateArguments
simulateArguments =
  SimulateArguments
    <$> treeFileOpt
    <*> phyloSubstitutionModelOpt
    <*> phyloMixtureModelOpt
    <*> maybeEDMFileOpt
    <*> maybeSiteprofilesFilesOpt
    <*> maybeMixtureWeights
    <*> maybeGammaParams
    <*> lengthOpt
    <*> seedOpt

treeFileOpt :: Parser FilePath
treeFileOpt =
  strOption $ long "tree-file" <> short 't' <> metavar "Name" <> help
    "Read tree from Newick file NAME"

phyloSubstitutionModelOpt :: Parser (Maybe String)
phyloSubstitutionModelOpt =
  optional
    $  strOption
    $  long "substitution-model"
    <> short 's'
    <> metavar "MODEL"
    <> help
         "Set the phylogenetic substitution model; available models are shown below (mutually exclusive with -m option)"

phyloMixtureModelOpt :: Parser (Maybe String)
phyloMixtureModelOpt = optional $ strOption
  (  long "mixture-model"
  <> short 'm'
  <> metavar "MODEL"
  <> help
       "Set the phylogenetic mixture model; available models are shown below (mutually exclusive with -s option)"
  )

maybeEDMFileOpt :: Parser (Maybe FilePath)
maybeEDMFileOpt = optional $ strOption
  (long "edm-file" <> short 'e' <> metavar "NAME" <> help
    "Empirical distribution model file NAME in Phylobayes format"
  )

-- fn :: Parsec Void String FilePath
-- fn = space *> takeWhile1P () <* space

-- fns :: Parsec Void String [FilePath]
-- fns = do
--   many string

maybeSiteprofilesFilesOpt :: Parser (Maybe [FilePath])
maybeSiteprofilesFilesOpt = optional $ words <$> strOption
  (long "siteprofile-files" <> short 'p' <> metavar "NAMES" <> help
    "File names of site profiles in Phylobayes format"
  )

maybeMixtureWeights :: Parser (Maybe [Double])
maybeMixtureWeights = optional $ option
  auto
  (  long "mixture-model-weights"
  <> short 'w'
  <> metavar "\"[DOUBLE,DOUBLE,...]\""
  <> help "Weights of mixture model components"
  )

maybeGammaParams :: Parser (Maybe GammaRateHeterogeneityParams)
maybeGammaParams = optional $ option
  auto
  (  long "gamma-rate-heterogeneity"
  <> short 'g'
  <> metavar "\"(NCAT,SHAPE)\""
  <> help "Number of gamma rate categories and shape parameter"
  )

lengthOpt :: Parser Int
lengthOpt = option
  auto
  (long "length" <> short 'l' <> metavar "NUMBER" <> help
    "Set alignment length to NUMBER"
  )

-- | The model specification is somewhat complicated, so we need to provide
-- additional help.
simulateFooter :: String
simulateFooter = intercalate "\n" $ sms ++ mms
 where
  sms =
    [ "Substitution models:"
    , "-s \"MODEL[PARAMETER,PARAMETER,...]{STATIONARY_DISTRIBUTION}\""
    , "   Supported DNA models: JC, F81, HKY, GTR4."
    , "     For example,"
    , "       -s HKY[KAPPA]{DOUBLE,DOUBLE,DOUBLE,DOUBLE}"
    , "       -s GTR4[e_AC,e_AG,e_AT,e_CG,e_CT,e_GT]{DOUBLE,DOUBLE,DOUBLE,DOUBLE}"
    , "          where the 'e_XY' are the exchangeabilities from nucleotide X to Y."
    , "   Supported Protein models: Poisson, Poisson-Custom, LG, LG-Custom, WAG, WAG-Custom, GTR20."
    , "     MODEL-Custom means that only the exchangeabilities of MODEL are used,"
    , "     and a custom stationary distribution is provided."
    , "     For example,"
    , "       -s LG"
    , "       -s LG-Custom{...}"
    , "       -s GTR20[e_AR,e_AN,...]{...}"
    , "          the 'e_XY' are the exchangeabilities from amino acid X to Y (alphabetical order)."
    , "   Notes: The F81 model for DNA is equivalent to the Poisson-Custom for proteins."
    , "          The GTR4 model for DNA is equivalent to the GTR20 for proteins."
    ]
  mms =
    [ ""
    , "Mixture models:"
    , "-m \"MIXTURE(SUBSTITUTION_MODEL_1,SUBSTITUTION_MODEL_2[PARAMETERS]{STATIONARY_DISTRIBUTION},...)\""
    , "   For example,"
    , "     -m \"MIXTURE(JC,HKY[6.0]{0.3,0.2,0.2,0.3})\""
    , "Mixture weights have to be provided with the -w option."
    , ""
    , "Special mixture models:"
    , "-m CXX"
    , "   where XX is 10, 20, 30, 40, 50, or 60; CXX models, Quang et al., 2008."
    , "-m \"EDM(EXCHANGEABILITIES)\""
    , "   Arbitrary empirical distribution mixture (EDM) models."
    , "   Stationary distributions have to be provided with the -e option."
    , "   For example,"
    , "     LG exchangeabilities with stationary distributions given in FILE."
    , "     -m \"EDM(LG-Custom)\" -e FILE"
    , "For special mixture models, mixture weights are optional."
    ]
