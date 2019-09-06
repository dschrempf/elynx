{- |
Module      :  OptionsTreeAna
Description :  Tree analysis options
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Aug 29 08:16:45 2019.

-}

module OptionsTreeAna
  ( parseArguments
  ) where

import           ELynx.Tools.Options

desc :: [String]
desc = [ "Analyze phylogenetic trees." ]

parseArguments :: IO GlobalArguments
parseArguments = parseArgumentsWith desc [] globalArguments
