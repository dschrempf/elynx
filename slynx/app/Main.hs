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

import qualified Simulate.Options           as S
import           Simulate.Simulate

import           ELynx.Tools.Logger

main :: IO ()
main = do
  a <- S.parseArgs
  h <- setupLogger (Just $ S.argsOutFileBaseName a)
  let a' = a { S.argsLogHandle = h }
  runReaderT simulate a'
  closeLogger h
