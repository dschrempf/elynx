{- |
Module      :  EvoMod.ArgParse
Description :  Project overlapping argument parsers
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon Jan 28 14:46:10 2019.

-}

module EvoMod.ArgParse
  ( parseEvoModArgs
  , programHeader
  ) where

import           Data.Version                    (showVersion)
import           Options.Applicative
import           Options.Applicative.Help.Pretty
import           Paths_evomod                    (version)
import           System.Environment


import           EvoMod.Data.Alphabet.Alphabet

evoModVersion :: String
evoModVersion = "EvoMod version " ++ showVersion version ++ "."

evoModCopyright :: String
evoModCopyright = "Developed by Dominik Schrempf."

evoModDescription :: String
evoModDescription = "Parse, view, modify and simulate evolutionary sequences and phylogenetic trees. The goal of EvoMod is reproducible research. Nothing is assumed about the data (e.g., the type of code), and no default values set. Everything has to be stated by the user. This leads to some work overhead in the beginning, but usually pays off in the end."

evoModHeaders :: [String]
evoModHeaders = [ evoModVersion
                , evoModCopyright
                ]

-- | A short header to be used in executables.
evoModHeader :: String
evoModHeader = unlines evoModHeaders

evoModHeaderDoc :: Doc
evoModHeaderDoc = vcat $ map pretty evoModHeaders

evoModFooterDoc :: Doc
evoModFooterDoc = vcat $ map pretty evoModFooters

-- | TODO: Check if this is good to set across executables.
evoModFooters :: [String]
evoModFooters = [ "File formats:" ] ++ fs ++
                [ "", "Alphabet types:" ] ++ as ++
                [ "", "Substitution models:" ] ++ sm
  where
    toListItem = ("  - " ++)
    fs = map toListItem ["FASTA"]
    as = map (toListItem . codeNameVerbose) [(minBound :: Code) ..]
    sm =  [ "  - HKY model with transition to transversion ratio kappa and a state frequency vector."
          , "    Specified with \"-m HKY[DOUBLE][DOUBLE,DOUBLE,DOUBLE,DOUBLE]\"."
          , "  - GTR model with five rate parameters and state frequency vector."
          , "    Specified with \"-m HKY[DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE][DOUBLE,DOUBLE,DOUBLE,DOUBLE]\"."
          , ""
          , "Note: The state frequency vector has to sum up to 1.0 and only has three free parameters." ]

versionOpt :: Parser (a -> a)
versionOpt = infoOption evoModHeader
  ( long "version"
    <> short 'v'
    <> help "Show version"
    <> hidden )

-- | Read the arguments and prints out help if needed.
parseEvoModArgs :: Parser a -> IO a
parseEvoModArgs p = execParser $
  info (helper <*> versionOpt <*> p)
  (fullDesc
    <> progDesc evoModDescription
    <> headerDoc (Just evoModHeaderDoc)
    <> footerDoc (Just evoModFooterDoc))

-- | Program header.
programHeader :: IO ()
programHeader = do
  p  <- getProgName
  as <- getArgs
  putStr evoModHeader
  putStrLn $ "Command line: " ++ p ++ " " ++ unwords as
