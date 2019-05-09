{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  EvoMod.Options
Description :  Global command line options and arguments.
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May  3 18:20:11 2019.

-}

module EvoMod.Options
  (
    -- * Header
    programHeader
    -- * Options parser
  , parseArgsWith
    -- * Reusable options
  , Verbosity (..)
  , verbosityOpt
  , seedOpt
  , outFileBaseNameOpt
  ) where

import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Version                    (showVersion)
import           Data.Word
import           Language.Haskell.TH
import           Options.Applicative
import           Options.Applicative.Help.Pretty
import           System.Environment

import           EvoMod.Tools.Misc
import           Paths_evomod_tools              (version)

-- Be careful; it is necessary to synchronize the evomod-xxx libraries, so that
-- the version number of evomod-tools matches the others.
versionString :: String
versionString = "EvoMod suite version " ++ showVersion version ++ "."

copyrightString :: String
copyrightString = "Developed by Dominik Schrempf."

compilationString :: String
compilationString = "Compiled on "
                    ++ $(stringE =<< runIO
                         ( formatTime defaultTimeLocale "%B %-e, %Y, at %H:%M %P, %Z."
                           `fmap` Data.Time.getCurrentTime ))

-- A short header to be used in executables. 'unlines' doesn't work here because
-- it adds an additional newline at the end :(.
hdr :: String
hdr = intercalate "\n" [ versionString
                       , copyrightString
                       , compilationString
                       ]

description :: String
description = "The goal of the EvoMod suite is reproducible research. Evolutionary sequences and phylogenetic trees can be read, viewed, modified and simulated without assuming anything about the data (e.g., the type of code), and without default values. The exact command with all arguments has to be stated by the user and are logged consistently. This leads to some work overhead in the beginning, but usually pays off in the end."

-- | Short, globally usable program header.
programHeader :: IO String
programHeader = do
  p  <- getProgName
  as <- getArgs
  return $ unlines [hdr, "Command line: " ++ p ++ " " ++ unwords as]

versionOpt :: Parser (a -> a)
versionOpt = infoOption hdr
  ( long "version"
    -- Lower case 'v' clashes with verbosity.
    <> short 'V'
    <> help "Show version"
    <> hidden )

-- | Read arguments with globally provided description, header, footer, and so
-- on. Custom additional description (first argument) and footer (second
-- argument) can be provided. print help if needed.
parseArgsWith :: Maybe [String] -> Maybe [String] -> Parser a -> IO a
parseArgsWith md mf p = execParser $
  info (helper <*> versionOpt <*> p)
  (fullDesc
    <> header hdr
    <> progDesc dsc'
    -- <> footerDoc (Just . (vcat . map pretty) $ ftr'))
    <> footerDoc (Just . (vcat . map pretty) $ ftr'))
  where
    dsc' = maybe description (\d -> unlines $ d ++ [description]) md
    ftr' = fromMaybe [] mf

-- | Verbosity levels.
data Verbosity = Quiet | Info | Debug
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

-- | Boolean option; be verbose; default NO.
verbosityOpt :: Parser Verbosity
verbosityOpt = option auto
  ( long "verbosity"
    <> short 'v'
    <> metavar "VALUE"
    <> value Info
    <> showDefault
    <> help ("Be verbose; one of: " ++ unwords (map show vs) ))
  where
    vs = allValues :: [Verbosity]

-- Difficult to handle with the verbosity option. I decided to go with a
-- verbosity data type that includes Quiet.
-- -- | Boolean option; be quiet; default NO.
-- quietOpt :: Parser Bool
-- quietOpt = switch
--   ( long "quiet"
--     <> short 'q'
--     <> showDefault
--     <> help "Be quiet; incompatible with -v" )

-- | Seed option for MWC. Defaults to RANDOM.
seedOpt :: Parser (Maybe [Word32])
seedOpt = optional $ option auto
  ( long "seed"
    <> short 'S'
    <> metavar "[INT]"
    <> help ("Seed for random number generator; "
             ++ "list of 32 bit integers with up to 256 elements (default: random)" ) )

-- | Output filename.
outFileBaseNameOpt :: Parser FilePath
outFileBaseNameOpt = strOption
  ( long "output-file-basename"
    <> short 'o'
    <> metavar "NAME"
    <> help "Specify base name of output file")
