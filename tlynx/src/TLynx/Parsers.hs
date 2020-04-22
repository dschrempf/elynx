{- |
Module      :  TLynx.Parsers
Description :  Common parsers
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Wed Apr 22 13:34:35 2020.

-}

module TLynx.Parsers
  ( NewickFormat
  , newickFormat
  , newickHelp
  )
where

import           Options.Applicative

import           ELynx.Import.Tree.Newick       ( NewickFormat(..)
                                                , description
                                                )
import           ELynx.Tools                    ( allValues )

-- | Parse 'NewickFormat'.
newickFormat :: Parser NewickFormat
newickFormat =
  option auto
    $  long "newick-format"
    <> short 'f'
    <> metavar "FORMAT"
    <> value Standard
    <> showDefault
    <> help "Newick tree format; see 'tlynx --help'"

-- | Help for different 'NewickFormat's.
newickHelp :: [String]
newickHelp = map (toListItem . description) (allValues :: [NewickFormat])
  where toListItem = ("- Newick " ++)
