{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  ELynx.Tools.Options
Description :  Global command line options and arguments
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri May  3 18:20:11 2019.

-}

module ELynx.Tools.Options
  (
    -- * Log file
    logHeader
  , logFooter
    -- * Options
  , parseArgumentsWith
  , Verbosity (..)
  , GlobalArguments (..)
  , globalArguments
  , seedOpt
    -- * Options meta
  , megaReadM
    -- * Formatting
  , fillParagraph
  ) where

import           Data.List                       hiding (group)
import           Data.Time
import           Data.Version                    (showVersion)
import           Data.Void
import           Data.Word
import           Language.Haskell.TH
import           Options.Applicative             hiding (empty)
import           Options.Applicative.Help.Pretty
import           System.Environment
import           Text.Megaparsec                 (Parsec, errorBundlePretty,
                                                  runParser)

import           ELynx.Tools.Misc
import           Paths_elynx_tools               (version)

-- Be careful; it is necessary to synchronize the evomod-xxx libraries, so that
-- the version number of elynx-tools matches the others.
versionString :: String
versionString = "ELynx Suite version " ++ showVersion version ++ "."

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

time :: IO String
time = formatTime defaultTimeLocale "%B %-e, %Y, at %H:%M %P, %Z." `fmap` Data.Time.getCurrentTime

-- | Short, globally usable string preceding all logs with obligatory description.
logHeader :: String -> IO String
logHeader desc = do
  t  <- time
  p  <- getProgName
  as <- getArgs
  return $ intercalate "\n"
    [ "----------------------------------------------------------------------"
    , "-- " <> desc
    , hdr
    , "Time: " ++ t
    , "Command line: " ++ p ++ " " ++ unwords as ]

-- | See 'logHeader' but at the end.
logFooter :: IO String
logFooter = do
  t <- time
  return $ "Time: " ++ t

versionOpt :: Parser (a -> a)
versionOpt = infoOption hdr
  ( long "version"
    -- Lower case 'v' clashes with verbosity.
    <> short 'V'
    <> help "Show version"
    <> hidden )

evoModSuiteFooter :: [Doc]
evoModSuiteFooter =
  [ empty
  , bold $ text "The ELynx Suite."
  , fillParagraph "A Haskell library and a tool set for computational biology. The goal of the ELynx Suite is reproducible research. Evolutionary sequences and phylogenetic trees can be read, viewed, modified and simulated without assuming anything about the data (e.g., the type of code), and without default values. The exact command with all arguments has to be stated by the user and is logged automatically. This leads to some work overhead in the beginning, but usually pays off in the end."
  , fill 9 (bold $ text "seq-ana")   <+> text "View, examine, and modify evolutionary sequences."
  , fill 9 (bold $ text "seq-sim")   <+> text "Simulate evolutionary sequences."
  , fill 9 (bold $ text "tree-ana")  <+> text "Analyze phylogenetic trees."
  , fill 9 (bold $ text "tree-dist") <+> text "Compute distances between phylogenetic trees."
  , fill 9 (bold $ text "tree-sim")  <+> text "Simulate phylogenetic trees." ]

-- | Read arguments with globally provided description, header, footer, and so
-- on. Custom additional description (first argument) and footer (second
-- argument) can be provided. print help if needed.
parseArgumentsWith :: [String] -> [String] -> Parser a -> IO a
parseArgumentsWith desc ftr p = execParser $
  info (helper <*> versionOpt <*> p)
  (fullDesc
    <> header hdr
    <> progDesc dsc'
    <> footerDoc (Just ftr'))
  where
    dsc' = unlines desc
    ftr' = vsep $ map pretty ftr ++ evoModSuiteFooter

-- | Verbosity levels.
data Verbosity = Quiet | Warning | Info | Debug
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

data GlobalArguments = GlobalArguments
  { verbosity       :: Verbosity
  , inFile          :: Maybe FilePath
  , outFileBaseName :: Maybe FilePath }

globalArguments :: Parser GlobalArguments
globalArguments = GlobalArguments
  <$> verbosityOpt
  <*> optional inFileOpt
  <*> optional outFileBaseNameOpt

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

-- TODO: Remove this option! Move into sub commands.
inFileOpt :: Parser FilePath
inFileOpt = strOption
  ( long "in-file"
    <> short 'i'
    <> metavar "FILENAME"
    <> help "Input file name" )

-- | Output filename.
outFileBaseNameOpt :: Parser FilePath
outFileBaseNameOpt = strOption
  ( long "output-file-basename"
    <> short 'o'
    <> metavar "NAME"
    <> help "Specify base name of output file")

-- | Seed option for MWC. Defaults to RANDOM.
seedOpt :: Parser (Maybe [Word32])
seedOpt = optional $ option auto
  ( long "seed"
    <> short 'S'
    <> metavar "[INT]"
    <> help ("Seed for random number generator; "
             ++ "list of 32 bit integers with up to 256 elements (default: random)" ) )

-- | See 'eitherReader', but for Megaparsec.
megaReadM :: Parsec Void String a -> ReadM a
megaReadM p = eitherReader $ \input ->
  let eea = runParser p "" input
  in
    case eea of
      Left eb -> Left $ errorBundlePretty eb
      Right a -> Right a

-- | Fill a string so that it becomes a paragraph with line breaks. Useful for
-- descriptions, headers and footers.
fillParagraph :: String -> Doc
fillParagraph = fillSep . map text . words

