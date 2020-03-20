{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      :  ELynx.Tools.Reproduction
Description :  Functions to ease reproduction of analyses
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Nov 19 15:07:09 2019.

Use of standard input is not supported.

TODO: Also store output hash.

TODO: Split validate: (1) check input files; (2) perform analysis and check output hash.

TODO: Divide module.

-}

module ELynx.Tools.Reproduction
  ( ELynx
    -- * Log file
  , logHeader
  , logFooter
    -- * Options
  , parseArgumentsWith
  , Verbosity(..)
  , toLogLevel
  , Force(..)
  , Arguments(..)
  , GlobalArguments(..)
  , globalArguments
  , createSubCommand
  , Seed(..)
  , seedOpt
    -- * Options meta
  , megaReadM
    -- * Formatting
  , fillParagraph
  -- * Reproduction
  , Reproducible(..)
  , State(..)
  , readR
  , writeR
  -- * Re-exports.
  , Generic
  , ToJSON
  )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , eitherDecodeFileStrict'
                                                , encodeFile
                                                )
import           Control.Monad                  ( zipWithM
                                                , void
                                                )
import           Data.ByteString.Base16         ( encode )
import           Crypto.Hash.SHA256             ( hash )
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString.Char8         as B
import           Data.Either                    ( either )
import           Data.Word
import           Data.Vector.Unboxed            ( Vector )
import           GHC.Generics                   ( Generic )
import           Options.Applicative            ( Parser
                                                , briefDesc
                                                , defaultPrefs
                                                , execParserPure
                                                , getParseResult
                                                , info
                                                )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           Control.Monad.Logger           ( LoggingT
                                                , LogLevel(..)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Data.List               hiding ( group )
import           Data.Time
import           Data.Version                   ( showVersion )
import           Data.Void
import           Language.Haskell.TH
import           Options.Applicative     hiding ( empty )
import           Options.Applicative.Help.Pretty
import           Text.Megaparsec                ( Parsec
                                                , errorBundlePretty
                                                , runParser
                                                )

import           ELynx.Tools.Misc
import           Paths_elynx_tools              ( version )

import           Debug.Trace                    ( traceShow )

-- | Logging transformer to be used with all executables.
type ELynx a = ReaderT (Arguments a) (LoggingT IO)

-- Be careful; it is necessary to synchronize the version numbers across packages.
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
-- it adds an additional newline at the end.
hdr :: String
hdr = intercalate "\n" [versionString, copyrightString, compilationString]

time :: IO String
time =
  formatTime defaultTimeLocale "%B %-e, %Y, at %H:%M %P, %Z."
    `fmap` Data.Time.getCurrentTime

-- | Short, globally usable string preceding all logs with obligatory description.
logHeader :: String -> IO String
logHeader desc = do
  t  <- time
  p  <- getProgName
  as <- getArgs
  -- let l = length desc
  return $ intercalate
    "\n"
    -- [ replicate (l+4) '-'
    [ "=== " <> desc
    , hdr
    , "Start time: " ++ t
    , "Command line: " ++ p ++ " " ++ unwords as
    ]

-- | See 'logHeader' but footer.
logFooter :: IO String
logFooter = do
  t <- time
  let timeStr = "=== End time: " ++ t
      -- l       = length timeStr
  return $ intercalate "\n" [timeStr]
    -- , replicate l '-' ]

versionOpt :: Parser (a -> a)
versionOpt = infoOption
  hdr
  (  long "version"
    -- Lower case 'v' clashes with verbosity.
  <> short 'V'
  <> help "Show version"
  <> hidden
  )

evoModSuiteFooter :: [Doc]
evoModSuiteFooter =
  [ empty
  , text "The ELynx Suite"
  , text "---------------"
  , fillParagraph
    "A Haskell library and a tool set for computational biology. The goal of the ELynx Suite is reproducible research. Evolutionary sequences and phylogenetic trees can be read, viewed, modified and simulated. Exact specification of all options is necessary, and nothing is assumed about the data (e.g., the type of code). The command line with all arguments is consistently, and automatically logged."
  , empty
  , fill 9 (text "slynx")
    <+> text "Analyze, modify, and simulate evolutionary sequences."
  , fill 9 (text "tlynx")
    <+> text "Analyze, modify, and simulate phylogenetic trees."
  , empty
  , text "Get help for specific commands:"
  , text "  slynx examine --help"
  ]

argumentsParser :: Parser a -> Parser (Arguments a)
argumentsParser p = helper <*> versionOpt <*> p'
  where p' = Arguments <$> globalArguments <*> p

-- | Parse arguments. Provide a global description, header, footer, and so on.
-- Custom additional description (first argument) and footer (second argument)
-- can be provided. print help if needed.
parseArgumentsWith :: [String] -> [String] -> Parser a -> IO (Arguments a)
parseArgumentsWith desc ftr p = execParser $ info
  (argumentsParser p)
  (fullDesc <> header hdr <> progDesc (unlines desc) <> footerDoc (Just ftr'))
  where ftr' = vsep $ map pretty ftr ++ evoModSuiteFooter

-- | Verbosity levels.
data Verbosity = Quiet | Warning | Info | Debug
  deriving (Show, Read, Eq, Enum, Bounded, Ord, Generic)

instance ToJSON Verbosity

instance FromJSON Verbosity

-- | Conert verbosity option to log level.
toLogLevel :: Verbosity -> LogLevel
toLogLevel Quiet   = LevelError
toLogLevel Warning = LevelWarn
toLogLevel Info    = LevelInfo
toLogLevel Debug   = LevelDebug

-- | Exit when output exists, or overwrite.
newtype Force = Force Bool
  deriving (Eq, Show, Generic)

instance ToJSON Force

instance FromJSON Force

-- | Argument skeleton to be used with all commands.
data Arguments a = Arguments { global :: GlobalArguments
                             , local  :: a
                             }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Arguments a)

instance FromJSON a => FromJSON (Arguments a)

instance Reproducible a => Reproducible (Arguments a) where
  inFiles     = inFiles . local
  outSuffixes = outSuffixes . local
  getSeed     = getSeed . local
  setSeed (Arguments g l) s = Arguments g $ setSeed l s
  parser  = argumentsParser (parser @a)
  cmdName = cmdName @a
  cmdDesc = cmdDesc @a
  cmdFtr  = cmdFtr @a

-- | A set of global arguments used by all programs. The idea is to provide a
-- common framework for shared arguments.
--
data GlobalArguments = GlobalArguments
  { verbosity       :: Verbosity
  , outFileBaseName :: Maybe FilePath
  , forceReanalysis :: Force }
  deriving (Eq, Show, Generic)

instance ToJSON GlobalArguments

instance FromJSON GlobalArguments

-- | See 'GlobalArguments', parser function.
globalArguments :: Parser GlobalArguments
globalArguments =
  GlobalArguments <$> verbosityOpt <*> optional outFileBaseNameOpt <*> redoOpt

-- | Create a sub command; convenience function.
createSubCommand
  :: forall a b . Reproducible a => (a -> b) -> Mod CommandFields b
createSubCommand f = command (cmdName @a) $ info
  (f <$> parser @a)
  (fullDesc <> progDesc (cmdDesc @a) <> footerDoc (pretty <$> cmdFtr @a))

-- | Boolean option; be verbose; default NO.
verbosityOpt :: Parser Verbosity
verbosityOpt = option
  auto
  (  long "verbosity"
  <> short 'v'
  <> metavar "VALUE"
  <> value Info
  <> showDefault
  <> help ("Be verbose; one of: " ++ unwords (map show vs))
  )
  where vs = allValues :: [Verbosity]

-- | Output filename.
outFileBaseNameOpt :: Parser FilePath
outFileBaseNameOpt = strOption
  (long "output-file-basename" <> short 'o' <> metavar "NAME" <> help
    "Specify base name of output file"
  )

redoOpt :: Parser Force
redoOpt = flag
  (Force False)
  (Force True)
  (long "force" <> short 'f' <> help
    "Ignore previous analysis and overwrite existing output files."
  )

-- | Random or fixed seed.
data Seed = Random | Fixed (Vector Word32)
  deriving (Eq, Show, Generic)

instance ToJSON Seed

-- | Seed option for MWC. Defaults to Random.
seedOpt :: Parser Seed
seedOpt = toSeed <$> seedPar

toSeed :: Maybe (Vector Word32) -> Seed
toSeed Nothing  = Random
toSeed (Just w) = Fixed w

seedPar :: Parser (Maybe (Vector Word32))
seedPar = optional $ option
  auto
  (long "seed" <> short 'S' <> metavar "[INT]" <> help
    (  "Seed for random number generator; "
    ++ "list of 32 bit integers with up to 256 elements (default: random)"
    )
  )

-- | See 'eitherReader', but for Megaparsec.
megaReadM :: Parsec Void String a -> ReadM a
megaReadM p = eitherReader $ \input ->
  let eea = runParser p "" input
  in  case eea of
        Left  eb -> Left $ errorBundlePretty eb
        Right a  -> Right a

-- | Fill a string so that it becomes a paragraph with line breaks. Useful for
-- descriptions, headers and footers.
fillParagraph :: String -> Doc
fillParagraph = fillSep . map text . words

-- | Reproducible commands have
--   - a set of input files to be checked for consistency,
--   - a set of output suffixes which define output files to be checked for consistency,
--   - a function to get the seed, if available,
--   - a function to set the seed, if applicable,
--   - a parser to read the command line,
--   - a nice program name, description, and footer.
class Reproducible a where
  inFiles  :: a -> [FilePath]
  outSuffixes :: a -> [String]
  getSeed  :: a -> Maybe Seed
  setSeed  :: a -> Vector Word32 -> a
  parser   :: Parser a
  cmdName  :: String
  cmdDesc  :: String
  cmdFtr   :: Maybe String
  cmdFtr = Nothing

-- | Necessary information for a reproducible run. Notably, the input files are
-- checked for consistency!
data State a = State
  { progName      :: String        -- ^ Program name.
  , argsStr       :: [String]      -- ^ Command line arguments without program name.
  , files         :: [FilePath]    -- ^ File paths of used files.
  , checkSums     :: [String]      -- ^ SHA256 sums of used files.
  , reproducible  :: a             -- ^ Command argument.
  } deriving (Generic)

instance ToJSON a => ToJSON (State a) where

instance FromJSON a => FromJSON (State a)

parse :: Show a => [String] -> Parser a -> a
parse s p = case getParseResult res of
  Nothing ->
    traceShow res $ error $ "Could not parse command line arguments: " ++ show s
  Just a -> a
  where res = execParserPure defaultPrefs (info p briefDesc) s

-- Does the command line fit the provided command?
checkArgs
  :: forall a
   . (Eq a, Show a, Reproducible a)
  => State a
  -> IO (Either String ())
checkArgs s = do
  let r   = reproducible s
      p   = parser @a
      as  = argsStr s
      res = parse as p
  return $ if res /= reproducible s
    then Left $ unlines
      ["Command line string and command arguments do not fit:", show as, show r]
    else Right ()

-- Does the file match the base 16 checksum?
checkFile :: FilePath -> B.ByteString -> IO (Either String ())
checkFile fp h = do
  h' <- hashFile fp
  return $ if h' == h
    then Right ()
    else Left $ unlines
      [ "SHA256 sum does not match for a file:"
      , fp ++ " has check sum " ++ B.unpack h'
      , "Stored check sum is " ++ B.unpack h
      ]

-- | Check if command line arguments and files check sums are matching.
validate :: (Eq a, Show a, Reproducible a) => State a -> IO (Either String ())
validate s = do
  chA  <- checkArgs s
  chFs <- zipWithM checkFile (files s) (map B.pack $ checkSums s)
  let ch = sequence_ (chA : chFs)
  return $ first ("Failed validating the reproduction file.\n" ++) ch

-- | Read and validate ELynx reproduction file. Check consistency of arguments
-- and input files.
readR
  :: forall a
   . (Eq a, Show a, Reproducible a, FromJSON a)
  => FilePath
  -> IO (State a)
readR fp = do
  res <- eitherDecodeFileStrict' fp :: IO (Either String (State a))
  case res of
    Left err -> do
      putStrLn "Failed reading the ELynx reproduction file."
      putStrLn "The following error was encountered."
      error err
    Right r -> do
      ch <- validate r
      return $ either error (const r) ch

-- | Helper function.
hashFile :: FilePath -> IO B.ByteString
hashFile f = encode . hash <$> B.readFile f

-- | Write an ELynx reproduction file.
writeR
  :: forall a
   . (Eq a, Show a, Reproducible a, ToJSON a)
  => String
  -> a
  -> IO ()
writeR bn r = do
  pn <- getProgName
  as <- getArgs
  let outFs = map (bn ++) (outSuffixes r)
  let fs = inFiles r ++ outFs
  cs <- mapM hashFile fs
  let cs' = map B.unpack cs
      s   = State pn as fs cs' r
  void $ encodeFile (bn ++ ".elynx") s
