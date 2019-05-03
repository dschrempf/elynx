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
import           Paths_evomod_seq                (version)
import           System.Environment

evoModVersion :: String
evoModVersion = "EvoMod suite version " ++ showVersion version ++ "."

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

stringsToDoc :: [String] -> Doc
stringsToDoc = vcat . map pretty

evoModSuiteFooterEnd :: [String]
evoModSuiteFooterEnd =
  [ ""
  , evoModVersion
  , evoModCopyright ]

versionOpt :: Parser (a -> a)
versionOpt = infoOption evoModHeader
  ( long "version"
    <> short 'v'
    <> help "Show version"
    <> hidden )

-- | Read the arguments and prints out help if needed.
parseEvoModArgs :: [String] -> Parser a -> IO a
parseEvoModArgs ftr p = execParser $
  info (helper <*> versionOpt <*> p)
  (fullDesc
    <> progDesc evoModDescription
    <> footerDoc (Just . stringsToDoc $ ftr'))
  where ftr' = ftr ++ evoModSuiteFooterEnd

-- | Program header.
programHeader :: IO String
programHeader = do
  p  <- getProgName
  as <- getArgs
  return $ unlines [evoModHeader, "Command line: " ++ p ++ " " ++ unwords as]
