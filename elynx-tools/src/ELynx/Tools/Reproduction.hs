{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      :  ELynx.Tools.Reproduction
Description :  Functions to ease reproduction of analyses
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Nov 19 15:07:09 2019.

Use of standard input is not supported.

-}

module ELynx.Tools.Reproduction
  ( -- * Log file
    logHeader
  , logFooter
    -- * Options
  , Verbosity(..)
  , toLogLevel
  , Force(..)
  , forceOpt
  , GlobalArguments(..)
  , globalArguments
  , Seed(..)
  , seedOpt
  , Arguments(..)
  , parseArguments
  -- * Reproduction
  , ELynx
  , Reproducible(..)
  , getReproductionHash
  , Reproduction(..)
  , writeReproduction
  , hashFile
  -- * Misc
  , createCommandReproducible
  , createCommand
  , elynxParserInfo
  , megaReadM
  -- * Re-exports
  , Generic
  , FromJSON
  , ToJSON
  )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , encodeFile
                                                )
import           Control.Monad                  ( void )
import           Crypto.Hash.SHA256             ( hash )
import           Data.ByteString.Base16         ( encode )
import qualified Data.ByteString.Char8         as B
import           Data.Word
import           Data.Vector.Unboxed            ( Vector )
import           GHC.Generics                   ( Generic )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           Control.Monad.Logger           ( LoggingT
                                                , LogLevel(..)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Data.List               hiding ( group )
import           Data.Time
import           Data.Version                   ( Version
                                                , showVersion
                                                )
import           Data.Void
import           Language.Haskell.TH
import           Options.Applicative     hiding ( empty )
import           Options.Applicative.Help.Pretty
import           Text.Megaparsec                ( Parsec
                                                , errorBundlePretty
                                                , runParser
                                                )

import           ELynx.Tools.Misc

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
hdr :: [String]
hdr = [versionString, copyrightString, compilationString]

time :: IO String
time =
  formatTime defaultTimeLocale "%B %-e, %Y, at %H:%M %P, %Z."
    `fmap` Data.Time.getCurrentTime

-- | Short, globally usable string preceding all logs with obligatory description.
logHeader :: String -> [String] -> IO String
logHeader h dsc = do
  t  <- time
  p  <- getProgName
  as <- getArgs
  return
    $  intercalate "\n"
    $  ("=== " <> h)
    :  dsc
    ++ hdr
    ++ ["Start time: " ++ t, "Command line: " ++ p ++ " " ++ unwords as]

-- | See 'logHeader' but footer.
logFooter :: IO String
logFooter = do
  t <- time
  let timeStr = "=== End time: " ++ t
  return $ intercalate "\n" [timeStr]

versionOpt :: Parser (a -> a)
versionOpt = infoOption
  (intercalate "\n" hdr)
  (  long "version"
    -- Lower case 'v' clashes with verbosity.
  <> short 'V'
  <> help "Show version"
  <> hidden
  )

evoModSuiteFooter :: [Doc]
evoModSuiteFooter =
  [ empty
  , text "ELynx"
  , text "-----"
  , fillParagraph
    "A Haskell library and tool set for computational biology. The goal of ELynx is reproducible research. Evolutionary sequences and phylogenetic trees can be read, viewed, modified and simulated. The command line with all arguments is logged consistently, and automatically. Data integrity is verified using SHA256 sums so that validation of past analyses is possible without the need to recompute the result."
  , empty
  , fill 9 (text "slynx")
    <+> text "Analyze, modify, and simulate evolutionary sequences."
  , fill 9 (text "tlynx")
    <+> text "Analyze, modify, and simulate phylogenetic trees."
  , fill 9 (text "elynx") <+> text "Validate and redo past analyses."
  , empty
  , text "Get help for sub commands:"
  , text "  slynx examine --help"
  ]

-- | Verbosity levels.
data Verbosity = Quiet | Warning | Info | Debug
  deriving (Show, Read, Eq, Enum, Bounded, Ord, Generic)

instance FromJSON Verbosity

instance ToJSON Verbosity

-- | Conert verbosity option to log level.
toLogLevel :: Verbosity -> LogLevel
toLogLevel Quiet   = LevelError
toLogLevel Warning = LevelWarn
toLogLevel Info    = LevelInfo
toLogLevel Debug   = LevelDebug

-- | Exit when output exists, or overwrite.
newtype Force = Force Bool
  deriving (Eq, Show, Generic)

instance FromJSON Force

instance ToJSON Force

-- | A set of global arguments used by all programs. The idea is to provide a
-- common framework for shared arguments.
--
data GlobalArguments = GlobalArguments
  { verbosity       :: Verbosity
  , outFileBaseName :: Maybe FilePath
  , forceReanalysis :: Force }
  deriving (Eq, Show, Generic)

instance FromJSON GlobalArguments

instance ToJSON GlobalArguments

-- | See 'GlobalArguments', parser function.
globalArguments :: Parser GlobalArguments
globalArguments =
  GlobalArguments <$> verbosityOpt <*> optional outFileBaseNameOpt <*> forceOpt

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

-- | Force option parser.
forceOpt :: Parser Force
forceOpt = flag
  (Force False)
  (Force True)
  -- DO NOT CHANGE --force nor -f; they are used by 'elynx redo'.
  (long "force" <> short 'f' <> help
    "Ignore previous analysis and overwrite existing output files."
  )

-- | Random or fixed seed.
data Seed = Random | Fixed (Vector Word32)
  deriving (Show, Generic)

-- | Upon equality check, a random seed is not different from a fixed one.
instance Eq Seed where
  Random  == _       = True
  _       == Random  = True
  Fixed s == Fixed t = s == t

instance FromJSON Seed

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

-- | Argument skeleton to be used with all commands.
data Arguments a = Arguments { global :: GlobalArguments
                             , local  :: a
                             }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (Arguments a)

instance ToJSON a => ToJSON (Arguments a)

instance Reproducible a => Reproducible (Arguments a) where
  inFiles     = inFiles . local
  outSuffixes = outSuffixes . local
  getSeed     = getSeed . local
  setSeed (Arguments g l) s = Arguments g $ setSeed l s
  parser  = argumentsParser (parser @a)
  cmdName = cmdName @a
  cmdDsc  = cmdDsc @a
  cmdFtr  = cmdFtr @a

argumentsParser :: Parser a -> Parser (Arguments a)
argumentsParser p = Arguments <$> globalArguments <*> p

elynxParser :: Parser a -> Parser a
elynxParser p = helper <*> versionOpt <*> p

-- | Parse arguments. Provide a global description, header, footer, and so on.
-- Custom additional description (first argument) and footer (second argument)
-- can be provided. print help if needed.
parseArguments :: forall a . Reproducible a => IO (Arguments a)
parseArguments = execParser
  $ elynxParserInfo (cmdDsc @a) (cmdFtr @a) (argumentsParser $ parser @a)

-- | Logging transformer to be used with all executables.
type ELynx a = ReaderT (Arguments a) (LoggingT IO)

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
  cmdDsc  :: [String]
  cmdFtr   :: [String]
  cmdFtr = []

-- | A unique hash of the reproduction data type.
getReproductionHash :: forall a . Reproducible a => Reproduction a -> String
getReproductionHash r =
  B.unpack
    $  encode
    $  hash
    $  B.pack
    $  unlines
    $
  -- Reproduction.
       progName r
    :  argsStr r
    <> [showVersion (rVersion r)]
    <> files r
    <> checkSums r
  -- Reproducible.
    <> inFiles ri
    <> outSuffixes ri
    <> [cmdName @a]
    <> cmdDsc @a
    <> cmdFtr @a
  where ri = reproducible r

setHash :: Reproducible a => Reproduction a -> Reproduction a
setHash r = r { rHash = Just h } where h = getReproductionHash r


-- | Necessary information for a reproducible run. Notably, the input files are
-- checked for consistency!
data Reproduction a = Reproduction
  { progName         :: String        -- ^ Program name.
  , argsStr          :: [String]      -- ^ Command line arguments without program name.
  , rVersion         :: Version
  , rHash            :: Maybe String  -- ^ Unique hash; see 'getReproductionHash'.
  , files            :: [FilePath]    -- ^ File paths of used files.
  , checkSums        :: [String]      -- ^ SHA256 sums of used files.
  , reproducible     :: a             -- ^ Command argument.
  } deriving (Generic)

instance FromJSON a => FromJSON (Reproduction a)

instance ToJSON a => ToJSON (Reproduction a)

-- | Helper function.
hashFile :: FilePath -> IO B.ByteString
hashFile f = encode . hash <$> B.readFile f

-- | Write an ELynx reproduction file.
writeReproduction
  :: forall a
   . (Eq a, Show a, Reproducible a, ToJSON a)
  => String
  -> a
  -> IO ()
writeReproduction bn r = do
  pn <- getProgName
  as <- getArgs
  let outFs = map (bn ++) (outSuffixes r)
  let fs    = inFiles r ++ outFs
  cs <- mapM hashFile fs
  let cs' = map B.unpack cs
      s   = Reproduction pn as version Nothing fs cs' r
  void $ encodeFile (bn <> ".elynx") (setHash s)

-- | Create a command; convenience function.
createCommandReproducible
  :: forall a b . Reproducible a => (a -> b) -> Mod CommandFields b
createCommandReproducible f = command (cmdName @a) $ f <$> elynxParserInfo
  (cmdDsc @a)
  (cmdFtr @a)
  (parser @a)

-- | Create a command; convenience function.
createCommand
  :: String
  -> [String]
  -> [String]
  -> Parser a
  -> (a -> b)
  -> Mod CommandFields b
createCommand nm dsc ftr p f = command nm $ f <$> parserInfo dsc' ftr' p
 where
  dsc' = if null dsc then Nothing else Just $ vsep $ map pretty dsc
  ftr' = if null ftr then Nothing else Just $ vsep $ map pretty ftr

-- | ELynx parser info; convenience function.
elynxParserInfo :: [String] -> [String] -> Parser a -> ParserInfo a
elynxParserInfo dsc ftr = parserInfo dsc' ftr'
 where
  dsc' = if null dsc then Nothing else Just $ vsep $ map pretty dsc
  ftr' = Just $ vsep $ map pretty ftr ++ evoModSuiteFooter

-- Short version of ELynx parser info for sub commands.
parserInfo :: Maybe Doc -> Maybe Doc -> Parser a -> ParserInfo a
parserInfo dsc ftr p = info
  (elynxParser p)
  (fullDesc <> headerDoc (Just hdr') <> progDescDoc dsc <> footerDoc ftr)
  where hdr' = vsep $ map pretty hdr

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
