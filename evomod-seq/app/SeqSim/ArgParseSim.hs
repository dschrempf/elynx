{- |
Module      :  ArgParseSim
Description :  EvoModSim argument parsing.
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
  -e,--seed INT            Set seed for the random number generator; list of 32
                           bit integers with up to 256 elements (default: [0])
  -q,--quiet               Be quiet
  -o,--output-file NAME    Specify output file NAME


-}


module ArgParseSim
  ( EvoModSimArgs (..)
  , parseEvoModSimArgs
  ) where

import           Data.Word
import           Options.Applicative

import           EvoMod.ArgParse

-- -- Ugly convenience function to read in more complicated command line options
-- -- with megaparsec and optparse
-- -- (https://github.com/pcapriotti/optparse-applicative#option-readers).
-- megaReadM :: MegaParser a -> ReadM a
-- megaReadM p = eitherReader $ \input ->
--   let eea = runParser p "" input
--   in
--     case eea of
--       Left eb -> Left $ errorBundlePretty eb
--       Right a -> Right a

type GammaRateHeterogeneityParams = (Int, Double)

data EvoModSimArgs = EvoModSimArgs
  { argsTreeFile                     :: FilePath
  , argsMaybeSubstitutionModelString :: Maybe String
  , argsMaybeMixtureModelString      :: Maybe String
  , argsMaybeEDMFile                 :: Maybe FilePath
  , argsMaybeMixtureWeights          :: Maybe [Double]
  , argsMaybeGammaParams             :: Maybe GammaRateHeterogeneityParams
  , argsLength                       :: Int
  , argsMaybeSeed                    :: Maybe [Word32]
  , argsQuiet                        :: Bool
  , argsFileOut                      :: FilePath
  }

evoModSimArgs :: Parser EvoModSimArgs
evoModSimArgs = EvoModSimArgs
  <$> treeFileOpt
  <*> phyloSubstitutionModelOpt
  <*> phyloMixtureModelOpt
  <*> maybeEDMFileOpt
  <*> maybeMixtureWeights
  <*> maybeGammaParams
  <*> lengthOpt
  <*> maybeSeedOpt
  <*> quietOpt
  <*> fileOutOpt

treeFileOpt :: Parser FilePath
treeFileOpt = strOption
  ( long "tree-file"
    <> short 't'
    <> metavar "NAME"
    <> help "Specify tree file NAME" )

quietOpt :: Parser Bool
quietOpt = switch
  ( long "quiet"
  <> short 'q'
  <> help "Be quiet" )

fileOutOpt :: Parser FilePath
fileOutOpt = strOption
  ( long "output-file"
    <> short 'o'
    <> metavar "NAME"
    <> help "Specify output file NAME")

phyloSubstitutionModelOpt :: Parser (Maybe String)
phyloSubstitutionModelOpt = optional $ strOption
  ( long "substitution-model"
    <> short 's'
    <> metavar "MODEL"
    <> help "Set the phylogenetic substitution model; available models are shown below (mutually exclusive with -m option)" )

phyloMixtureModelOpt :: Parser (Maybe String)
phyloMixtureModelOpt = optional $ strOption
  ( long "mixture-model"
    <> short 'm'
    <> metavar "MODEL"
    <> help "Set the phylogenetic mixture model; available models are shown below (mutually exclusive with -s option)" )

maybeEDMFileOpt :: Parser (Maybe FilePath)
maybeEDMFileOpt = optional $ strOption
  ( long "edm-file"
    <> short 'e'
    <> metavar "NAME"
    <> help "empirical distribution model file NAME in Phylobayes format" )

maybeMixtureWeights :: Parser (Maybe [Double])
maybeMixtureWeights = optional $ option auto
  ( long "mixture-model-weights"
    <> short 'w'
    <> metavar "[DOUBLE,DOUBLE,...]"
    <> help "weights of mixture model components" )

maybeGammaParams :: Parser (Maybe GammaRateHeterogeneityParams)
maybeGammaParams = optional $ option auto
  ( long "gamma-rate-heterogeneity"
    <> short 'g'
    <> metavar "(NCAT, SHAPE)"
    <> help "number of gamma rate categories and shape parameter" )

lengthOpt :: Parser Int
lengthOpt = option auto
  ( long "length"
    <> short 'l'
    <> metavar "NUMBER"
    <> help "Set alignment length to NUMBER" )

maybeSeedOpt :: Parser (Maybe [Word32])
maybeSeedOpt = optional $ option auto
  ( long "seed"
    <> short 'S'
    <> metavar "[INT]"
    <> value [ 0 :: Word32 ]
    <> showDefault
    <> help ( "Set seed for the random number generator; "
              ++ "list of 32 bit integers with up to 256 elements" ) )

-- | Read the arguments and prints out help if needed.
parseEvoModSimArgs :: IO EvoModSimArgs
parseEvoModSimArgs = parseEvoModArgs evoModSimFooter evoModSimArgs

evoModSimFooter :: [String]
evoModSimFooter = sms ++ mms
  where
    sms =
      [ "Substitution models:"
      , "-s MODEL[PARAMETER,PARAMETER,...]{STATIONARY_DISTRIBUTION}"
      , "   Supported DNA models: JC, HKY."
      , "     For example,"
      , "       -s HKY[KAPPA]{DOUBLE,DOUBLE,DOUBLE,DOUBLE}"
      , "   Supported Protein models: Poisson, Poisson-Custom, LG, LG-Custom, WAG, WAG-Custom."
      , "     MODEL-Custom means that only the exchangeabilities of MODEL are used,"
      , "     and a custom stationary distribution is provided."
      , "     For example,"
      , "       -s LG-Custom{...}"
      ]
    mms =
      [ ""
      , "Mixture models:"
      , "-m MIXTURE(SUBSTITUTION_MODEL_1,SUBSTITUTION_MODEL_2)"
      , "   For example,"
      , "     -m MIXTURE(JC,HKY[6.0]{0.3,0.2,0.2,0.3})"
      , "Mixture weights have to be provided with the -w option."
      , ""
      , "Special mixture models:"
      , "-m CXX"
      , "   where XX is 10, 20, 30, 40, 50, or 60; CXX models, Quang et al., 2008."
      , "-m EDM(EXCHANGEABILITIES)"
      , "   Arbitrary empirical distribution mixture (EDM) models."
      , "   Stationary distributions have to be provided with the -e option."
      , "   For example,"
      , "     LG exchangeabilities with stationary distributions given in FILE."
      , "     -m EDM(LG-Custom) -e FILE"
      , "For special mixture models, mixture weights are optional."
      ]
