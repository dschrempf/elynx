{- |
Module      :  EvoMod.Definitions
Description :  Some global definitions
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 25 17:17:41 2019.

-}

module EvoMod.Definitions
  ( eps
  , precision
  , programHeader
  , parseArgsWith
  ) where

import           Data.Version                    (showVersion)
import           Options.Applicative
import           Options.Applicative.Help.Pretty
import           System.Environment

import           Paths_evomod_tools              (version)

-- | Required precision when comparing 'Double' values.
eps :: Double
eps = 1e-12

-- | Default output precision.
precision :: Int
precision = 3

-- Be careful; it is necessary to synchronize the evomod-xxx libraries, so that
-- the version number of evomod-tools matches the others.
versionString :: String
versionString = "EvoMod suite version " ++ showVersion version ++ "."

copyrightString :: String
copyrightString = "Developed by Dominik Schrempf."

description :: String
description = "Parse, view, modify and simulate evolutionary sequences and phylogenetic trees. The goal of EvoMod is reproducible research. Nothing is assumed about the data (e.g., the type of code), and no default values are set. Everything has to be stated by the user. Command lines are logged consistently. This leads to some work overhead in the beginning, but usually pays off in the end."

headers :: [String]
headers = [ versionString
          , copyrightString
          ]

-- A short header to be used in executables.
hdr :: String
hdr = unlines headers

-- | Short program header.
programHeader :: IO String
programHeader = do
  p  <- getProgName
  as <- getArgs
  return $ unlines [hdr, "Command line: " ++ p ++ " " ++ unwords as]

stringsToDoc :: [String] -> Doc
stringsToDoc = vcat . map pretty

ftr :: [String]
ftr =
  [ ""
  , versionString
  , copyrightString ]

versionOpt :: Parser (a -> a)
versionOpt = infoOption hdr
  ( long "version"
    <> short 'v'
    <> help "Show version"
    <> hidden )

-- | Read arguments with header and footer and print help if needed.
parseArgsWith :: [String] -> [String] -> Parser a -> IO a
parseArgsWith h f p = execParser $
  info (helper <*> versionOpt <*> p)
  (fullDesc
    <> progDesc hdr'
    <> footerDoc (Just . stringsToDoc $ ftr'))
  where ftr' = f ++ ftr
        hdr' = unlines $ description : h
