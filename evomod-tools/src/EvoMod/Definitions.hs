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

import           Data.Maybe
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

-- A short header to be used in executables.
hdr :: String
hdr = unlines [ versionString
              , copyrightString
              ]

description :: String
description = "The goal of the EvoMod suite is reproducible research. Evolutionary sequences and phylogenetic trees can be read, viewed, modified and simulated without assuming anything about the data (e.g., the type of code), and without default values. The exact command with all arguments has to be stated by the user and are logged consistently. This leads to some work overhead in the beginning, but usually pays off in the end."

-- | Short program header.
programHeader :: IO String
programHeader = do
  p  <- getProgName
  as <- getArgs
  return $ unlines [hdr, "Command line: " ++ p ++ " " ++ unwords as]

versionOpt :: Parser (a -> a)
versionOpt = infoOption hdr
  ( long "version"
    <> short 'v'
    <> help "Show version"
    <> hidden )

-- | Read arguments with possibly custom additional description and footer;
-- print help if needed.
parseArgsWith :: Maybe [String] -> Maybe [String] -> Parser a -> IO a
parseArgsWith md mf p = execParser $
  info (helper <*> versionOpt <*> p)
  (fullDesc
    <> header hdr
    <> progDesc dsc'
    <> footerDoc (Just . (vcat . map pretty) $ ftr'))
  where
    dsc' = maybe description (\d -> unlines $ d ++ [description]) md
    ftr' = fromMaybe [] mf
