{- |
Module      :  Main
Description :  Work with molecular sequence data
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep  5 21:53:07 2019.

-}

module Main where

import           Control.Monad.Trans.Reader

import           Simulate.Options
import           Simulate.Simulate

import           ELynx.Tools.Options
import           ELynx.Tools.Logger

main :: IO ()
main = do
  a <- parseArguments
  let f = outFileBaseName $ globalArgs a
      l = case f of
        Nothing -> runELynxStderrLoggingT simulate
        Just fn -> runELynxFileLoggingT (fn ++ ".log") simulate
  runReaderT l a

-- work :: Seq ()
-- work = do
--   c <- commandArgs <$> lift ask
--   h <- liftIO $ logHeader "seq-ana: Analyze sequences."
--   $(logInfo) $ T.pack h
--   case c of
--     Examine ps fp        -> examineCmd ps fp
--     Concatenate fps      -> concatenateCmd fps
--     FilterRows lo sh fp  -> filterRowsCmd lo sh fp
--     FilterColumns st fp  -> filterColumnsCmd st fp
--     SubSample ns na s fp -> subSampleCmd ns na s fp
--     Translate fr cd fp   -> translateCmd fr cd fp
