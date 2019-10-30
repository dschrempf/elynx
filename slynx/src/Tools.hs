{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Tools
Description :  Common tools for sequence lynx
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sat Sep  7 06:24:22 2019.

-}

module Tools
  ( -- * Tools
    readSeqs
    -- * Options
  , alphabetOpt
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Text                    as T
import           Options.Applicative

import           ELynx.Data.Alphabet.Alphabet
import           ELynx.Data.Sequence.Sequence
import           ELynx.Import.Sequence.Fasta  hiding (Parser)
import           ELynx.Tools.InputOutput

-- | Read sequences of given alphabet from file or standard input.
readSeqs :: (MonadIO m, MonadLogger m) => Alphabet -> Maybe FilePath -> m [Sequence]
readSeqs a mfp = do
  case mfp of
    Nothing -> $(logInfo) $ T.pack
               $ "Read sequences from standard input; alphabet "
               <> show a <> "."
    Just fp -> $(logInfo) $ T.pack
               $ "Read sequences from file "
               <> fp <> "; alphabet " <> show a <> "."
  liftIO $ parseFileOrIOWith (fasta a) mfp

-- | Command line option to specify the alphabet. Used by various commands.
alphabetOpt :: Parser Alphabet
alphabetOpt = option auto $
  long "alphabet" <>
  short 'a' <>
  metavar "NAME" <>
  help "Specify alphabet type NAME"
