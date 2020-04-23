{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Main
Description :  Validate elynx file
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Wed Apr 22 21:08:25 2020.

TODO: Help.

TODO: Check version number.

TODO: Think of a stable reproducible hash. The description and so on are not stable enough.

-}

module Main
  ( main
  ) where

import Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader     ( ask )
import Data.Aeson
import qualified Data.Aeson.Types as J
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Options.Applicative

import ELynx.Tools

import  qualified         SLynx.Options as S
import  qualified         TLynx.Options as T

data AllReproductions = S (Reproduction (Arguments S.CommandArguments))
                      | T (Reproduction (Arguments T.CommandArguments))

newtype ValidateArguments = ValidateArguments
  { elynxFile :: FilePath }
  deriving (Eq, Show, Generic)

instance Reproducible ValidateArguments where
  inFiles = const []
  outSuffixes = const []
  getSeed = const Nothing
  setSeed = const
  parser = validateArguments
  cmdName = "validate"
  cmdDsc = ["Validate a past ELynx analysis."]

instance FromJSON ValidateArguments

instance ToJSON ValidateArguments

validateArguments :: Parser ValidateArguments
validateArguments = ValidateArguments <$> inFileArg

inFileArg :: Parser FilePath
inFileArg = strArgument $ metavar "ELYNX-FILE"

parseProgName :: Value -> J.Parser String
parseProgName = withObject "progName" $ \o -> o .: "progName"

-- TODO: Declaring program names here should not be necessary.
parseAllR :: String -> Value -> J.Parser AllReproductions
parseAllR "slynx" v = S <$> (parseJSON v :: J.Parser (Reproduction (Arguments S.CommandArguments)))
parseAllR "tlynx" v = T <$> (parseJSON v :: J.Parser (Reproduction (Arguments T.CommandArguments)))
parseAllR p _ = let err = unlines ["Could not parse program name:"
                                  , p
                                  , "Do you use the correct ELynx version?"]
                in error err

parse :: Show a => [String] -> Parser a -> a
parse s p = fromMaybe
            (error $ "parse: could not parse command line arguments: " ++ show s)
            (getParseResult res)
  where res = execParserPure defaultPrefs (info p briefDesc) s

-- Does the command line fit the saved arguments?
checkArgs
  :: forall a
   . (Eq a, Show a, Reproducible a)
  => Reproduction a
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


-- Check if command line arguments and files check sums are matching.
validate :: (Eq a, Show a, Reproducible a) => Reproduction a -> IO (Either String ())
validate s = do
  chA  <- checkArgs s
  chFs <- zipWithM checkFile (files s) (map B.pack $ checkSums s)
  return $ sequence_ (chA : chFs)

validateAllReproductions :: AllReproductions -> IO (Either String ())
validateAllReproductions (S x) = validate x
validateAllReproductions (T x) = validate x

-- Read and validate ELynx reproduction file. Check consistency of arguments and
-- input files.
runValidate :: ELynx ValidateArguments ()
runValidate = do
  fp <- elynxFile . local <$> ask
  eELynx <- liftIO (eitherDecodeFileStrict fp :: IO (Either String Value))
  elynx  <- case eELynx of
              Left err -> do
                $(logWarn) "Failed decoding the ELynx reproduction file."
                $(logWarn) "The following error occurred:"
                error err
              Right val -> return val
  prog <- case J.parseEither parseProgName elynx of
          Left err -> do
            $(logWarn) "Failed getting command hash from ELynx reproduction file."
            $(logWarn) "The following error occurred:"
            error err
          Right prog -> return prog
  repr <- case J.parseEither (parseAllR prog) elynx of
            Left err -> do
              $(logWarn) "Failed parsing the ELynx reproduction file."
              $(logWarn) "The following error occurred:"
              error err
            Right repr -> return repr
  val <- liftIO $ validateAllReproductions repr
  case val of
    Left err -> do
      $(logWarn) "Failed validating the ELynx reproduction file."
      $(logWarn) "The following error occurred:"
      error err
    Right () -> $(logInfo) "Validation successful!"
  -- TODO: Offer re-analysis.

main :: IO ()
main = do
  a <- parseArguments
  eLynxWrapper a id runValidate
